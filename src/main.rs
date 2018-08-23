// Handwritten recursive descent parser for graphql queries to try out porting something I've written
// in c to rust.

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

// Idea,
// Have a specific parse error type

// is this like an "expect" type of thing? Or is that only for parsing?
fn get_char(query: &mut Chars) -> Result<char, &'static str> {
    match query.next() {
        Some(c) => Ok(c),
        None => Err("Parse Error: Unexpected EOF")
    }
}

fn next_token(query: &mut Peekable<Chars>) -> Result<Token, &'static str> {
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
                                '.' | 'e' | 'E' => {
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

fn main() {
    let query: &'static str = "abZc #comment \n  def \"a string\" 123 12.9e24 ... ! [] | 123 wuoah";
    let mut iter = query.chars().peekable();
    loop {
        match next_token(&mut iter) {
            Ok(token) => {
                println!("Token: {:?}", token);
                if token == Token::EOF {
                    break;
                }
            },
            Err(e) => {
                panic!("Error Parsing Stream: {}", e);
            }
        }
    }
}