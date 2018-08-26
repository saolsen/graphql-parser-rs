// Handwritten recursive descent parser for graphql queries to try out porting something I've written
// in c to rust.

// @TODO: Use a string intern table to speed up the parse.
// @TODO: Better errors.
#![feature(nll)]

#[macro_use] extern crate proptest;
use proptest::prelude::*;
use proptest::test_runner::Config;

use std::str::Chars;
use std::iter::Peekable;

#[derive(PartialEq, Debug)]
enum Token {
    EOF,
    FloatValue(f64),
    IntValue(i64),
    StringValue(String),
    Name(String),
    Punctuator(char),
    Ellipsis,
}

fn get_next_token(query: &mut Peekable<Chars>) -> Result<Token, &'static str> {
    // Skip Whitespace
    loop {
        match query.peek() {
            Some(',') | Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                query.next();
            },
            Some('#') => {
                query.next();
                loop {
                    match query.peek() {
                        None | Some('\n') => break,
                        _ => query.next()
                    };
                }
            }
            _ => break
        }
    }

    match query.peek().cloned() {
        None => Ok(Token::EOF),
        Some(c) => {
            match c {
                // Number
                '-' | '0'...'9' => {
                    // @TODO: Better way to do this? Don't really wanna allocate here.
                    let mut num = String::new();
                    num.push(c);
                    query.next();
                    let mut is_float = false;
                    loop {
                        if let Some(c) = query.peek().cloned() {
                            match c {
                                '0'...'9' => {
                                    num.push(c);
                                    query.next();
                                    continue;
                                },
                                '.' | 'e' | 'E' | '-' => {
                                    num.push(c);
                                    query.next();
                                    is_float = true;
                                    continue;
                                },
                                _ => ()
                            }
                        }
                        break;
                    }
                    if is_float {
                        if let Ok(f) = num.parse::<f64>() {
                            Ok(Token::FloatValue(f))
                        } else {
                            Err("Error parsing float value.")
                        }
                    } else {
                        if let Ok(i) = num.parse::<i64>() {
                            Ok(Token::IntValue(i))
                        } else {
                            Err("Error parsing int value.")
                        }
                    }
                },
                // Name
                // @TODO: intern strings
                'a'...'z' | 'A'...'Z' => {
                    let mut name = String::new();
                    name.push(c);
                    query.next();
                    loop {
                        if let Some(c) = query.peek().cloned() {
                            match c {
                                'a'...'z' | 'A'...'Z' | '0' ... '9' => {
                                    name.push(c);
                                    query.next();
                                    continue
                                }
                                _ => ()
                            }
                        }
                        break;
                    }
                    Ok(Token::Name(name))
                },
                // String
                '"' => {
                    let mut s = String::new();
                    query.next();
                    // @TODO: Handle escapes and block strings.
                    loop {
                        match query.peek().cloned() {
                            Some('"') | None => {
                                query.next();
                                break;
                            }
                            Some(c) => {
                                s.push(c);
                                query.next();
                            }
                        }
                    }
                    Ok(Token::StringValue(s))
                },
                // Punctuators
                '!' | '$' | '(' | ')' | ':' | '=' | '@' | '[' | ']' | '{' | '|' | '}' => {
                    query.next();
                    Ok(Token::Punctuator(c))
                },
                '.' => {
                    query.next();
                    let p2 = query.next();
                    let p3 = query.next();
                    if let (Some('.'),Some('.')) = (p2,p3) {
                        Ok(Token::Ellipsis)
                    } else {
                        Err("Error parsing ellipses.")
                    }
                },
                _ => Err("Error, unexpeced initial token.")
            }
        }
    }
}

struct Lexer<'a> {
    query: Peekable<Chars<'a>>,
    token: Token
}

impl<'a> Lexer<'a> {
    fn new(s: &'a str) -> Result<Self, &'static str> {
        let mut query = s.chars().peekable();
        let token = get_next_token(&mut query)?;
        Ok(Lexer { query, token })
    }

    pub fn next_token(&mut self) -> Result<(), &'static str> {
        let token = get_next_token(&mut self.query)?;
        self.token = token;
        Ok(())
    }

    pub fn expect_token(&mut self, t: Token) -> Result<(), &'static str> {
        if t != self.token {
            Err("Error: Expected t but got dis")
        } else {
            self.next_token()?;
            Ok(())
        }
    }

    pub fn token_name(&mut self) -> Result<String, &'static str> {
        if let Token::Name(ref s) = self.token {
            Ok(s.clone())
        } else {
            Err("Expected Token::Name")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex() {
        let query: &'static str = "abZc #comment \n  def \"a string\" 123 12.9e24 ... ! [] | 123 wuoah";
        let mut l = Lexer::new(query).unwrap();

        let mut assert_token = |token| {
            assert_eq!(l.token, token);
            l.next_token().unwrap();
        };

        assert_token(Token::Name("abZc".to_string()));
        assert_token(Token::Name("def".to_string()));
        assert_token(Token::StringValue("a string".to_string()));
        assert_token(Token::IntValue(123));
        assert_token(Token::FloatValue(12.9e24));
        assert_token(Token::Ellipsis);
        assert_token(Token::Punctuator('!'));
        assert_token(Token::Punctuator('['));
        assert_token(Token::Punctuator(']'));
        assert_token(Token::Punctuator('|'));
        assert_token(Token::IntValue(123));
        assert_token(Token::Name("wuoah".to_string()));
        assert_token(Token::EOF);
    }
}

#[derive(PartialEq, Debug)]
enum TypeDef {
    List { typedef: Box<TypeDef> },
    NonNull { typedef: Box<TypeDef> },
    Named { name: String }
}

#[derive(PartialEq, Debug, Clone)]
enum Value {
    IntValue(i64),
    FloatValue(f64),
    Variable(String),
    StringValue(String),
    BoolValue(bool),
    NullValue,
    Enum(String),
    List(Vec<Value>),
    Object(Vec<(String,Value)>)
}

fn print_value(val: &Value) -> String {
    match val {
        Value::IntValue(i) => format!("{}", i),
        Value::FloatValue(f) => format!("{:.e}", f),
        Value::Variable(v) => format!("${}", v),
        Value::StringValue(s) => format!("\"{}\"", s),
        Value::BoolValue(true) => "true".to_string(),
        Value::BoolValue(false) => "false".to_string(),
        Value::NullValue => "null".to_string(),
        Value::Enum(e) => format!("{}", e),
        Value::List(l) => {
            let items = l.iter().map(|v| print_value(v)).collect::<Vec<String>>();
            format!("[{}]", items.join(", "))
        },
        Value::Object(o) => {
            let items = o.iter().map(|kvp| format!("{}: {}", kvp.0, print_value(&kvp.1))).collect::<Vec<String>>();
            format!("{{{}}}", items.join(", "))
        }
    }
}

#[derive(PartialEq, Debug)]
struct Argument {
    name: String,
    value: Value
}

#[derive(PartialEq, Debug)]
struct Directive {
    name: String,
    arguments: Vec<Argument>
}

#[derive(PartialEq, Debug)]
struct Fragment {
    name: String,
    on: String,
    directives: Vec<Directive>,
    selection_set: Vec<Selection>,
}

#[derive(PartialEq, Debug)]
enum Selection {
    Field {
        alias: Option<String>,
        name: String,
        arguments: Vec<Argument>,
        directives: Vec<Directive>,
        selection_set: Vec<Selection>
    },
    FragmentSpread {
        name: String,
        directives: Vec<Directive>
    },
    InlineFragment {
        on: String, // @TODO: Check this out.
        directives: Vec<Directive>,
        selection_set: Vec<Selection>
    }
}

#[derive(PartialEq, Debug)]
struct VariableDefinition {
    name: String,
    typedef: TypeDef,
    default_value: Option<Value>
}

#[derive(PartialEq, Debug)]
enum OperationKind {
    Query,
    Mutation,
    Subscription
}

#[derive(PartialEq, Debug)]
struct Operation {
    kind: OperationKind,
    name: Option<String>,
    variable_definitions: Vec<VariableDefinition>,
    directives: Vec<Directive>,
    selection_set: Vec<Selection>
}

#[derive(PartialEq, Debug)]
struct Document {
    fragments: Vec<Fragment>,
    operations: Vec<Operation>
}

fn parse_value(mut lexer: &mut Lexer) -> Result<Value, &'static str> {
    let v = match lexer.token {
        Token::Punctuator('$') => {
            lexer.next_token()?;
            Ok(Value::Variable(lexer.token_name()?))
        },
        Token::IntValue(i) => {
            Ok(Value::IntValue(i))
        },
        Token::FloatValue(f) => {
            Ok(Value::FloatValue(f))
        },
        Token::StringValue(ref s) => {
            Ok(Value::StringValue(s.clone()))
        },
        Token::Name(ref n) => {
            match n.as_str() {
                "true" => Ok(Value::BoolValue(true)),
                "false" => Ok(Value::BoolValue(false)),
                "null" => Ok(Value::NullValue),
                _ => Ok(Value::Enum(n.clone()))
            }
        },
        Token::Punctuator('[') => {
            lexer.next_token()?;
            let mut list = vec![];
            while lexer.token != Token::Punctuator(']') {
                list.push(parse_value(&mut lexer)?);
            }
            Ok(Value::List(list))
        },
        Token::Punctuator('{') => {
            lexer.next_token()?;
            let mut obj = vec![];
            while lexer.token != Token::Punctuator('}') {
                let name = lexer.token_name()?;
                lexer.next_token()?;
                lexer.expect_token(Token::Punctuator(':'))?;
                let val = parse_value(&mut lexer)?;
                obj.push((name,val));
            }
            Ok(Value::Object(obj))
        },
        _ => Err("Error parsing value")
    };
    lexer.next_token()?;
    v
}

// arguments
fn parse_arguments(mut lexer: &mut Lexer) -> Result<Vec<Argument>, &'static str> {
    let mut args = vec![];
    if lexer.token == Token::Punctuator('(') {
        lexer.next_token()?;
        while lexer.token != Token::Punctuator(')') {
            let name= lexer.token_name()?;
            lexer.next_token()?;
            lexer.expect_token(Token::Punctuator(':'))?;
            let value = parse_value(&mut lexer)?;
            args.push(Argument{name, value})
        }
        lexer.next_token()?;
    }
    Ok(args)
}

// directives
fn parse_directives(mut lexer: &mut Lexer) -> Result<Vec<Directive>, &'static str> {
    let mut directives = vec![];
    while lexer.token == Token::Punctuator('@') {
        lexer.next_token()?;
        let name = lexer.token_name()?;
        let arguments = parse_arguments(&mut lexer)?;
        directives.push(Directive{name, arguments})
    }
    Ok(directives)
}

fn parse_selection(mut lexer: &mut Lexer) -> Result<Selection, &'static str> {
    if lexer.token == Token::Ellipsis {
        lexer.next_token()?;
        match lexer.token {
            Token::Name(ref s) if s == "on" => {
                lexer.next_token()?;
                let on = lexer.token_name()?;
                lexer.next_token()?;
                let directives = parse_directives(&mut lexer)?;
                let selection_set = parse_selection_set(&mut lexer)?;
                Ok(Selection::InlineFragment {on, directives, selection_set })
            },
            _ => {
                let name = lexer.token_name()?;
                lexer.next_token()?;
                let directives = parse_directives(&mut lexer)?;
                Ok(Selection::FragmentSpread {name, directives })
            }
        }
    } else {
        let first_token = lexer.token_name()?;
        lexer.next_token()?;
        let mut alias = None;
        let name;
        if lexer.token == Token::Punctuator(':') {
            alias = Some(first_token);
            lexer.next_token()?;
            name = lexer.token_name()?;
            lexer.next_token()?;
        } else {
            name = first_token;
        }
        let arguments = parse_arguments(&mut lexer)?;
        let directives = parse_directives(&mut lexer)?;
        let selection_set = parse_selection_set(&mut lexer)?;
        Ok(Selection::Field {alias, name, arguments, directives, selection_set})
    }
}

fn parse_selection_set(mut lexer: &mut Lexer) -> Result<Vec<Selection>, &'static str> {
    let mut selection_set = vec![];
    if lexer.token == Token::Punctuator('{') {
        lexer.next_token()?;
        selection_set.push(parse_selection(&mut lexer)?);
        while lexer.token != Token::Punctuator('}') {
            selection_set.push(parse_selection(&mut lexer)?);
        }
        lexer.next_token()?;
    }
    Ok(selection_set)
}

fn parse_type_def(mut lexer: &mut Lexer) -> Result<TypeDef, &'static str> {
    let t = if lexer.token == Token::Punctuator('[') {
        // list type
        lexer.next_token()?;
        let list_of = parse_type_def(&mut lexer)?;
        lexer.expect_token(Token::Punctuator(']'))?;
        TypeDef::List {typedef: Box::new(list_of) }
    } else {
        // named type
        let name = lexer.token_name()?;
        lexer.next_token()?;
        TypeDef::Named { name }
    };

    if lexer.token == Token::Punctuator('!') {
        lexer.next_token()?;
        Ok(TypeDef::NonNull {typedef: Box::new(t)})
    } else {
        Ok(t)
    }
}

fn parse_variable_definitions(mut lexer: &mut Lexer) -> Result<Vec<VariableDefinition>, &'static str> {
    let mut variable_definitions = vec![];
    if lexer.token == Token::Punctuator('(') {
        lexer.next_token()?;
        while lexer.token != Token::Punctuator(')') {
            lexer.expect_token(Token::Punctuator('$'))?;
            let name = lexer.token_name()?;
            lexer.next_token()?;
            lexer.expect_token(Token::Punctuator(':'))?;
            let typedef = parse_type_def(&mut lexer)?;
            let default_value = match lexer.token {
                Token::Punctuator('$') | Token::Punctuator(')') => {
                    None
                },
                _ => {
                    lexer.expect_token(Token::Punctuator('='))?;
                    Some(parse_value(&mut lexer)?)
                }
            };
            variable_definitions.push(VariableDefinition{name, typedef, default_value})
        }
    }
    Ok(variable_definitions)
}

fn parse_operation_definition(mut lexer: &mut Lexer) -> Result<Operation, &'static str> {
    let mut kind = OperationKind::Query;
    let mut name = None;
    let mut variable_definitions = vec![];
    let mut directives = vec![];

    if let Token::Name(ref s) = lexer.token {
        match s.as_str() {
            "query" => kind = OperationKind::Query,
            "mutation" => kind = OperationKind::Mutation,
            "subscription" => kind = OperationKind::Subscription,
            _ => return Err("Error, unknown operation")
        }
        lexer.next_token()?;

        if let Token::Name(ref s) = lexer.token {
            name = Some(s.clone());
            lexer.next_token()?;
        }

        variable_definitions = parse_variable_definitions(&mut lexer)?;
        directives = parse_directives(&mut lexer)?;
    }

    let selection_set = parse_selection_set(&mut lexer)?;

    Ok(Operation{
        kind,
        name,
        variable_definitions,
        directives,
        selection_set
    })
}

fn parse_fragment_definition(mut lexer: &mut Lexer) -> Result<Fragment, &'static str> {
    if lexer.token_name()? != "fragment" {
        return Err("Error: Expected 'fragment'");
    }
    lexer.next_token()?;
    let name = lexer.token_name()?;
    if name == "on" {
        return Err("Error: fragment can not be named 'on'");
    }
    lexer.next_token()?;
    lexer.expect_token(Token::Name("on".to_string()))?;
    let on = lexer.token_name()?;
    lexer.next_token()?;
    let directives = parse_directives(&mut lexer)?;
    let selection_set = parse_selection_set(&mut lexer)?;
    Ok(Fragment{name, on, directives, selection_set})
}

fn parse_query(query: &str) -> Result<Document, &'static str> {
    let mut lexer = Lexer::new(query)?;

    let mut fragments = Vec::new();
    let mut operations = Vec::new();

    while lexer.token != Token::EOF {
        match lexer.token {
            Token::Name(ref s) if s == "fragment" => {
                fragments.push(parse_fragment_definition(&mut lexer)?)
            },
            Token::Name(_) | Token::Punctuator('{') => {
                operations.push(parse_operation_definition(&mut lexer)?)
            }
            _ => {
                return Err("Unknown Initial Token")
            }
        }
    }
    Ok(Document {fragments, operations})
}

/* prop_compose! {
    fn gen_value()(v in any::<i64>()) -> Value {
        Value::IntValue(v)
    }
} */

fn gen_value() -> BoxedStrategy<Value> {
    let leaf = prop_oneof![
        any::<i64>().prop_map(Value::IntValue),
        any::<f64>().prop_map(Value::FloatValue),
        "([A-Z][a-z])([A-Z][a-z][0-9])*".prop_map(Value::Variable),
        "[^\"]*".prop_map(Value::StringValue),
        any::<bool>().prop_map(Value::BoolValue),
        Just(Value::NullValue),
        "([A-Z][a-z])([A-Z][a-z][0-9])*".prop_map(Value::Enum)
    ];
    leaf.prop_recursive(
        3,
        50,
        10,
        |inner| prop_oneof![
            prop::collection::vec(inner.clone(), 0..10)
                .prop_map(Value::List),
            prop::collection::hash_map("([A-Z][a-z])([A-Z][a-z][0-9])*", inner, 0..10)
                .prop_map(|vals|
                    Value::Object(
                        vals.iter().map(|(k,v)| (k.to_owned(), v.to_owned()))
                        .collect())
                    )
        ]).boxed()
}

proptest! {
    //#![proptest_config(Config::with_cases(10000))]
    #[test]
    fn test_parse_value(v in gen_value()) {
        let s = print_value(&v);
        let mut lex = Lexer::new(&s).unwrap();
        let parsed_v = parse_value(&mut lex).unwrap();
        assert!(parsed_v == v);
    }

    #[test]
    fn test_parse_query(s in "\\PC*") {
        parse_query(&s)
    }

    #[test]
    fn test_parse_int_value(ref v in gen_value()) {
        //println!("{:#?}", v);
        if let Value::IntValue(val) = v {
            assert!(*val != 1024)
        }
    }
}

fn main() {
    let value = Value::Object(vec![]);
    let s = print_value(&value);
    println!("{}", s);
    let mut lex = Lexer::new(&s).unwrap();
    let parsed = parse_value(&mut lex).unwrap();
    println!("{:?}", parsed);
}
