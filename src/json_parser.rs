/// 这儿可能有问题
type Input = &'static str;
type MyStr = &'static str;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEof,
    ExpectedEof(Input),
    UnexpectedChar(char),
    UnexpectedString(MyStr),
}

pub type ParseResult<T> = std::result::Result<(Input, T), ParseError>;


/// 解析器
trait Parser<A> {
    fn parse(&self, input: Input) -> ParseResult<A>;
}


/// 常见解析器
struct UnexpectedCharParser(char);

impl<A> Parser<A> for UnexpectedCharParser {
    fn parse(&self, _: Input) -> ParseResult<A> {
        Err(ParseError::UnexpectedChar(self.0))
    }
}

struct ConstantParser<A>(ParseResult<A>);

impl<A> Parser<A> for ConstantParser<A> where A : Clone{
    fn parse(&self, _: Input) -> ParseResult<A> {
        self.0.clone()
    }
}

struct CharacterParser;
impl Parser<char> for CharacterParser {
    fn parse(&self, input: Input) -> ParseResult<char> {
        if let Some(c) = input.chars().next() {
            Ok((unsafe { input.get_unchecked(1..input.len())}, c))
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
}

struct ValueParser<A>(A);
impl<A> Parser<A> for ValueParser<A> where A : Clone {
    fn parse(&self, input: Input) -> ParseResult<A> {
        Ok((input, self.0.clone()))
    }
}

struct OrParser<A>(Box<dyn Parser<A>>, Box<dyn Parser<A>>);

impl<A> Parser<A> for OrParser<A> {
    fn parse(&self, input: Input) -> ParseResult<A> {
        let result =  (*self.0).parse(input);
        if result.is_err() {
            (*self.1).parse(input)
        } else {
            result
        }
    }
}

/// 解析A和B, 然后丢弃A
struct DropAParser<A, B>(Box<dyn Parser<A>>, Box<dyn Parser<B>>);

impl<A, B> Parser<B> for DropAParser<A, B> {
    fn parse(&self, input: Input) -> ParseResult<B> {
        let (input1, _) = (*self.0).parse(input)?;
        let result = (*self.1).parse(input1)?;
        Ok(result)
    }
}

/// SatisfyParser :: (char -> bool) -> Parser<char>
struct SatisfyParser(Box<dyn Fn(char) -> bool>);
impl Parser<char> for SatisfyParser {
    fn parse(&self, input: Input) -> ParseResult<char> {
        if let Some(c) = input.chars().next() {
            if (*self.0)(c) {
                Ok((unsafe { input.get_unchecked(1..input.len()) }, c))
            } else {
                Err(ParseError::UnexpectedChar(c))
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
}

struct IsParser(SatisfyParser);

impl IsParser {
    fn new(c: char) -> Self {
        IsParser(SatisfyParser(Box::new(move |c_| c == c_)))
    }
}

impl Parser<char> for IsParser {
    fn parse(&self, input: Input) -> ParseResult<char> {
        self.0.parse(input)
    }
}

struct Digit(SatisfyParser);

impl Digit {
    fn new() -> Self {
        Digit(SatisfyParser(Box::new(|c| c.is_numeric())))
    }
}

impl Parser<char> for Digit {
    fn parse(&self, input: Input) -> ParseResult<char> {
        self.0.parse(input)
    }
}


struct Space(SatisfyParser);

impl Space {
    fn new() -> Self {
        Space(SatisfyParser(Box::new(|c| c.is_whitespace())))
    }
}

impl Parser<char> for Space {
    fn parse(&self, input: Input) -> ParseResult<char> {
        self.0.parse(input)
    }
}

struct AppendParser<A>(Box<dyn Parser<A>>, Box<dyn Parser<Vec<A>>>);


impl<A> Parser<Vec<A>> for AppendParser<A> where A: Clone {
    fn parse(&self, input: Input) -> ParseResult<Vec<A>> {
        let result = (*self.0).parse(input)?;
        let result1 = (*self.1).parse(result.0)?;
        let head = vec![result.1];
        Ok((result1.0, vec![head, result1.1].concat()))
    }
}

struct List0Parser<A>(Box<dyn Parser<A>>);
struct List1Parser<A>(Box<dyn Parser<A>>);


fn parse_zero_or_more<A>(p: &dyn Parser<A>, input: Input) -> (Input, Vec<A>) where A : Clone {
    let result = parse_one_or_more(p, input);
    match result {
        Ok(res@_) => res,
        Err(_) => (input, Vec::new())
    }
}

fn parse_one_or_more<A>(p: &dyn Parser<A>, input: Input) -> ParseResult<Vec<A>> where A : Clone {
    let result = (*p).parse(input)?;
    let result1 = parse_zero_or_more(p, result.0);
    let head = vec![result.1];
    Ok((result1.0, vec![head, result1.1].concat()))
}

impl<A> Parser<Vec<A>> for List0Parser<A> where A : Clone {
    fn parse(&self, input: Input) -> ParseResult<Vec<A>> {
        Ok(parse_zero_or_more(&*self.0, input))
    }
}

impl<A> Parser<Vec<A>> for List1Parser<A> where A : Clone {
    fn parse(&self, input: Input) -> ParseResult<Vec<A>> {
        parse_one_or_more(&*self.0, input)
    }
}

fn spaces() -> List0Parser<char> {
    List0Parser(Box::new(Space::new()))
}

fn space_1() -> List1Parser<char> {
    List1Parser(Box::new(Space::new()))
}


fn lower() -> SatisfyParser {
    SatisfyParser(Box::new(|c| c.is_lowercase()))
}

fn upper() -> SatisfyParser {
    SatisfyParser(Box::new(|c| c.is_uppercase()))
}

fn alpha() -> SatisfyParser {
    SatisfyParser(Box::new(|c| c.is_alphabetic()))
}



struct Tok<A>(Box<dyn Parser<A>>);

// lazy_static! {
//     static ref space: Box<Space> = Box::new(Space::new());
// }

impl<A> Parser<A> for Tok<A> {
    fn parse(&self, input: Input) -> ParseResult<A> {
        let result = (*self.0).parse(input)?;
        let after = parse_zero_or_more(&Space::new(), result.0);
        Ok((after.0, result.1))
    }
}

fn char_tok(c: char) -> Tok<char> {
    Tok(Box::new(IsParser::new(c)))
}

fn comma_tok() -> Tok<char> {
    Tok(Box::new(IsParser::new(',')))
}

fn quote() -> impl Parser<char> {
    OrParser(Box::new(IsParser::new('"')), Box::new(IsParser::new('\'')))
}

struct StringParser(&'static str);

impl Parser<String> for StringParser {
    fn parse(&self, input: Input) -> ParseResult<String> {
        let mut result = (input, String::from(""));
        let mut chars = self.0.chars();
        while let Some(c) = chars.next() {
            let (tmp_input, c) = IsParser::new(c).parse(result.0)?;
            result.1.push(c);
            result.0 = tmp_input;
        }
        Ok(result)
    }
}

fn string_tok(s: &'static str) -> Tok<String> {
    Tok(Box::new(StringParser(s)))
}

fn option<A>(a: A, p: Box<dyn Parser<A>>) -> impl Parser<A> where A: Clone, A: 'static {
    OrParser(p, Box::new(ValueParser(a)))
}

fn digits1() -> List1Parser<char> {
    List1Parser(Box::new(Digit::new()))
}

fn one_of(s: &'static str) -> SatisfyParser {
    SatisfyParser(Box::new(move |c| s.contains(c)))
}

fn none_of(s: &'static str) -> SatisfyParser {
    SatisfyParser(Box::new(move |c| !s.contains(c)))
}

struct Between<Left, Center, Right>(Box<dyn Parser<Left>>, Box<dyn Parser<Center>>, Box<dyn Parser<Right>>);

impl<Left, Center, Right> Parser<Center> for Between<Left, Center, Right> {
    fn parse(&self, input: Input) -> ParseResult<Center> {
        let (input1, _) = (*self.0).parse(input)?;
        let (input2, center) = (*self.1).parse(input1)?;
        let (input3, _) = (*self.2).parse(input2)?;
        Ok((input3, center))
    }
}

fn bewteen_char_tok<A>(left: char, right: char, p: Box<dyn Parser<A>>) -> impl Parser<A>  {
    Between(
        Box::new(char_tok(left)),
        p,
        Box::new(char_tok(right))
    )
}

struct Hex;

impl Parser<char> for Hex {
    fn parse(&self, input: Input) -> ParseResult<char> {
        let p = SatisfyParser(Box::new(|c| c.is_ascii_hexdigit()));
        let (i_1, b_1) = p.parse(input)?;
        let (i_2, b_2) = p.parse(i_1)?;
        let (i_3, b_3) = p.parse(i_2)?;
        let (i_4, b_4) = p.parse(i_3)?;
        // 这儿会产生溢出
        let mut sum: u8 = 0;
        sum += b_4 as u8;
        sum += (b_3 as u8) * 16;
        sum += (b_2 as u8) * 16 * 16;
        sum += (b_1 as u8) * 16 * 16 * 16;
        Ok((i_4, sum as char))
    }
}

fn hexu() -> impl Parser<char> {
    DropAParser(Box::new(IsParser::new('u')), Box::new(Hex))
}


struct SepBy1<A, S>(Box<dyn Parser<A>>, Box<dyn Parser<S>>);
impl<A, S> Parser<Vec<A>> for SepBy1<A, S> {
    fn parse(&self, input: Input) -> ParseResult<Vec<A>> {
        let mut result = (input, Vec::new());
        let (input1, a) = (*self.0).parse(input)?;
        result.0 = input1;
        result.1.push(a);
        while let Ok((tmp_input, _)) = (*self.1).parse(result.0) {
            let (input1, a) = (*self.0).parse(tmp_input)?;
            result.0 = input1;
            result.1.push(a);
        }
        Ok(result)
    }
}

fn sep_by<A, S>(p: Box<dyn Parser<A>>, s: Box<dyn Parser<S>>) -> OrParser<Vec<A>> where A: Clone, A: 'static, S: 'static {
    OrParser(Box::new(SepBy1(p, s)), Box::new(ValueParser(Vec::new())))
}

struct Eof;
impl Parser<()> for Eof {
    fn parse(&self, input: Input) -> ParseResult<()> {
        if input.len() == 0 {
            Ok((input, ()))
        } else {
            Err(ParseError::UnexpectedString(input))
        }
    }
}

fn between_sep_by_comma<A>(left: char, right: char, p: Box<dyn Parser<A>>) -> impl Parser<Vec<A>> where A: Clone, A: 'static {
    bewteen_char_tok(left, right, Box::new(sep_by(p, Box::new(char_tok(',')))))
} 


// json parser

#[derive(Debug)]
enum SpecialChar {
    BackSpace,
    FormFeed,
    NewLine,
    CarriageReturn,
    Tab,
    SingleQuote,
    DoubleQuote,
    BackSlash,
}

fn from_special_char(sc: SpecialChar) -> char {
    match sc {
        SpecialChar::BackSpace => 8 as char,
        SpecialChar::FormFeed => 12 as char,
        SpecialChar::NewLine => '\n',
        SpecialChar::CarriageReturn => '\r',
        SpecialChar::Tab => '\t',
        SpecialChar::SingleQuote => '\'',
        SpecialChar::DoubleQuote => '\"',
        SpecialChar::BackSlash => '\\'
    }
}

fn to_special_char(c: char) -> Option<SpecialChar> {
    match c {
        'b' => Some(SpecialChar::BackSpace),
        'f' => Some(SpecialChar::FormFeed),
        'n' => Some(SpecialChar::NewLine),
        'r' => Some(SpecialChar::CarriageReturn),
        't' => Some(SpecialChar::Tab),
        '\'' => Some(SpecialChar::SingleQuote),
        '\"' => Some(SpecialChar::DoubleQuote),
        '\\' => Some(SpecialChar::BackSlash),
        _ => None
    }
}

struct StringBody;
impl Parser<char> for StringBody {
    fn parse(&self, input: Input) -> ParseResult<char> {
        let (input1, c1) = CharacterParser.parse(input)?;
        if c1 == '\\' {
            let (input2, c2) = CharacterParser.parse(input1)?;
            if c2 == 'u' {
                let result = Hex.parse(input2)?;
                Ok(result)
            } else {
                if let Some(sc) = to_special_char(c2) {
                    Ok((input2, from_special_char(sc)))
                } else {
                    Err(ParseError::UnexpectedChar(c2))
                }
            }
        } else {
            if c1 == '"' {
                Err(ParseError::UnexpectedChar(c1))
            } else {
                Ok((input1, c1))
            }
        }
    }
}

fn json_string() -> impl Parser<Vec<char>> {
    Between(
        Box::new(IsParser::new('"')),
        Box::new(List0Parser(Box::new(StringBody))),
        Box::new(char_tok('"'))
    )
}



struct Number;
// "1.232e+3"
// z: +, a: 1, b: ., c: 232, d: e, e: +, f: 3
impl Parser<Vec<char>> for Number {
    fn parse(&self, input: Input) -> ParseResult<Vec<char>> {
        let p_num_one_or_more= List1Parser(Box::new(Digit::new()));
        let p_flag = OrParser(Box::new(IsParser::new('+')), Box::new(IsParser::new('-')));
        let p_flag_with_default = OrParser(Box::new(p_flag), Box::new(ValueParser('+')));

        let (input0, z) = p_flag_with_default.parse(input)?;
        let (input1, a) = p_num_one_or_more.parse(input0)?;
        let dot = IsParser::new('.').parse(input1);
        match dot {
            Ok((input2, b)) => {
                let (input3, c) = p_num_one_or_more.parse(input2)?;
                let exp = IsParser::new('e').parse(input3);
                match exp {
                    Ok((input4, d)) => {
                        let (input5, e) = p_flag_with_default.parse(input4)?;
                        let (input6, f) = p_num_one_or_more.parse(input5)?;
                        Ok((input6, [vec![z], a, vec![b], c, vec![d], vec![e], f].concat()))
                    },
                    Err(_) => Ok((input3, [vec![z], a, vec![b], c].concat()))
                }
            },
            Err(_) => Ok((input1, [vec![z], a].concat()))
        }
    }
}

fn str_to_num(str: String) -> f64 {
    match str.parse::<f64>() {
        Ok(f) => f,
        Err(_) => panic!("this won't be happen")
    } 
}

struct JsonNumber;
impl Parser<f64> for JsonNumber {
    fn parse(&self, input: Input) -> ParseResult<f64> {
        let (input1, vec) = Number.parse(input)?;
        let str: String = vec.into_iter().collect();
        Ok((input1, str_to_num(str)))
    }
}

struct JsonTrue;
impl Parser<String> for JsonTrue {
    fn parse(&self, input: Input) -> ParseResult<String> {
        string_tok("true").parse(input)
    }
}

struct JsonFalse;
impl Parser<String> for JsonFalse {
    fn parse(&self, input: Input) -> ParseResult<String> {
        string_tok("false").parse(input)
    }
}

struct JsonNull;
impl Parser<String> for JsonNull {
    fn parse(&self, input: Input) -> ParseResult<String> {
        string_tok("null").parse(input)
    }
}

#[derive(Debug, Clone)]
pub enum Json {
    JsonNull,
    JsonTrue,
    JsonFalse,
    JsonArray(Vec<Json>),
    JsonString(String),
    JsonObject(Vec<(String, Json)>),
    JsonNumber(f64)
}

struct JsonArray;
impl Parser<Vec<Json>> for JsonArray {
    fn parse(&self, input: Input) -> ParseResult<Vec<Json>> {
        between_sep_by_comma('[', ']', Box::new(JsonValue)).parse(input)
    }
}

struct KeyValue;
impl Parser<(String, Json)> for KeyValue {
    fn parse(&self, input: Input) -> ParseResult<(String, Json)> {
        let (input1, key) = json_string().parse(input)?;
        let (input2, _) = char_tok(':').parse(input1)?;
        let (input3, json) = JsonValue.parse(input2)?;
        Ok((input3, (key.into_iter().collect(), json)))
    }
}

struct JsonObject;
impl Parser<Vec<(String, Json)>> for JsonObject {
    fn parse(&self, input: Input) -> ParseResult<Vec<(String, Json)>> {
        between_sep_by_comma('{', '}', Box::new(KeyValue)).parse(input)
    }
}

struct JsonValue;
impl Parser<Json> for JsonValue {
    fn parse(&self, input: Input) -> ParseResult<Json> {
        let (trimmed, _) = spaces().parse(input)?;
        if let Ok(r) = JsonNull.parse(trimmed) {
            Ok((r.0, Json::JsonNull))
        } else if let Ok(r) = JsonTrue.parse(trimmed) {
            Ok((r.0, Json::JsonTrue))
        } else if let Ok(r) = JsonFalse.parse(trimmed) {
            Ok((r.0, Json::JsonFalse))
        } else if let Ok(r) = JsonArray.parse(trimmed) {
            Ok((r.0, Json::JsonArray(r.1)))
        } else if let Ok(r) = json_string().parse(trimmed) {
            Ok((r.0, Json::JsonString(r.1.into_iter().collect())))
        } else if let Ok(r) = JsonObject.parse(trimmed) {
            Ok((r.0, Json::JsonObject(r.1)))
        } else if let Ok(r) = JsonNumber.parse(trimmed) {
            Ok((r.0, Json::JsonNumber(r.1)))
        } else {
            Err(ParseError::UnexpectedString(trimmed))
        }

    }
}

pub fn parse(s: &'static str) -> ParseResult<Json> {
    JsonValue.parse(s)
}

pub fn test() {
    let input: Input = "";
    let result = OrParser(Box::new(CharacterParser), Box::new(ValueParser('v'))).parse("");
    println!("result = {:?}", result);

    let result2 = SatisfyParser(Box::new(|c|c.is_uppercase())).parse("Abc");
    println!("result2 = {:?}", result2);

    let digit = Digit::new();
    println!("{:?}, {:?}", digit.parse("123"), digit.parse("hello"));

    let cp = IsParser::new('c');
    println!("{:?}", cp.parse("cccs"));


    let result3 = AppendParser(Box::new(Digit::new()), Box::new(ValueParser(vec!['h', 'i']))).parse("321");
    println!("{:?}", result3);

    let digit2 = Box::new(Digit::new());
    let lp = List1Parser(digit2);
    let result4 = lp.parse("1234abns");
    println!("{:?}", result4);

    let tok = Tok(Box::new(IsParser::new('a')));
    println!("{:?}", tok.parse("a       bc"));

    let str_parser = StringParser("Hello Rust");
    println!("{:?}", str_parser.parse("Hello Rust wqkjekjwqekjwq"));

    let op_parser = option('x', Box::new(CharacterParser));
    println!("{:?}, {:?}", op_parser.parse("abc"), op_parser.parse(""));

    let parse_num = "1.232e+3".parse::<f64>();
    match parse_num {
        Ok(n) => println!("parse_num = {}", n),
        Err(e) => println!("{}", e)
    }

    let one_of_p = one_of("abc");
    println!("one-of abc from bcdef: {:?}", one_of_p.parse("bcdef"));

    // let between = Between(
    //     Box::new(IsParser::new('[')),
    //     Box::new(CharacterParser),
    //     Box::new(IsParser::new(']'))
    // );
    let between = bewteen_char_tok('[', ']', Box::new(CharacterParser));

    println!("[a]: {:?}, [abc]: {:?}, [a: {:?}", between.parse("[a]"), between.parse("[abc]"), between.parse("[a"));


    let sep_by = SepBy1(Box::new(CharacterParser), Box::new(IsParser::new(',')));
    println!("sep_by a,b,c = {:?}", sep_by.parse("a,b,c"));

    let ps = between_sep_by_comma('[', ']', Box::new(lower()));
    println!("arr parse  [a, b, c] = {:?}", ps.parse("[a, b, c]"));
    let ps2 = between_sep_by_comma('[', ']', Box::new(digits1()));
    println!("arr parse  [123, 456] = {:?}", ps2.parse("[123, 456]"));

    let num = JsonNumber;
    println!("{:?}", num.parse("1.324e-12abc"));
    println!("{:?}", num.parse("1.324e-12abc"));
    println!("{:?}", num.parse("1234sd"));
    println!("{:?}", num.parse("-1.32-4e12abc"));
    println!("{:?}", num.parse("-1234"));

    let json =  "{ \"key1\" : true , \"key2\" : [7, false] }";
    println!("{:?}", JsonValue.parse(json));
}


