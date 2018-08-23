// Handwritten recursive descent parser for graphql queries to try out porting something I've written
// in c to rust.

use std::str::Chars;

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

fn next_token(next_ch: Option<char>, query: &mut Chars) -> Result<(Option<char>, Token), &'static str> {
    

    // @TODO: Use peek instead of this.
    // https://adriann.github.io/rust_parser.html
    let mut ch = next_ch;

    // Skip Whitespace
    loop {
        match ch {
            Some(',') | Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                ch = query.next();
            },
            Some('#') => {
                ch = query.next();
                loop {
                    match ch {
                        None | Some('\n') => break,
                        _ => ch = query.next()
                    }
                }
            }
            _ => break
        }
    }

    match ch {
        None => Ok((None, Token::EOF)),
        Some(c) => {
            match c {
                // Number
                d @ '-' | d @ '0' ... '9' => {
                    // @TODO: Better way to do this? Don't really wanna allocate here.
                    let mut num = String::new();
                    num.push(d);
                    ch = query.next();
                    let mut is_float = false;
                    loop {
                        if let Some(c) = ch {
                            match c {
                                d @ '0' ... '9' => {
                                    num.push(d);
                                    ch = query.next();
                                    continue;
                                },
                                d @ '.' | d @ 'e' | d @ 'E' => {
                                    num.push(d);
                                    ch = query.next();
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
                           Ok((ch,Token::FloatValue(f)))
                        } else {
                            Err("Error parsing float value.")
                        }
                    } else {
                        if let Ok(i) = num.parse::<i64>() {
                            Ok((ch,Token::IntValue(i)))
                        } else {
                            Err("Error parsing int value.")
                        }
                    }
                },

                // Name
                // @TODO: intern strings
                l @ 'a'...'z' | l @ 'A'...'Z' => {
                    let mut name = String::new();
                    name.push(l);
                    ch = query.next();
                    loop {
                        if let Some(c) = ch {
                            match c {
                                l @ 'a'...'z' | l @ 'A'...'Z' | l @ '0' ... '9' => {
                                    name.push(l);
                                    ch = query.next();
                                    continue
                                }
                                _ => ()
                            }
                        }
                        break;
                    }
                    Ok((ch,Token::Name(name)))
                },

                // String
                '"' => {
                    let mut s = String::new();
                    ch = query.next();
                    // @TODO: Handle escapes and block strings.
                    loop {
                        match ch {
                            Some('"') | None => {
                                ch = query.next();
                                break;
                            }
                            Some(c) => {
                                s.push(c);
                                ch = query.next();
                            }
                        }
                    }
                    Ok((ch,Token::StringValue(s)))
                },

                // Punctuators
                p @ '!' | p @ '$' | p @ '(' | p @ ')' | p @ ':' | p @ '=' | p @ '@' | p @ '[' | p @ ']' | p @ '{' | p @ '|' | p @ '}'  =>
                    Ok((query.next(),Token::Punctuator(p))),
                '.' => {
                    let n = query.next();
                    let nn = query.next();
                    ch = query.next();
                    if let (Some('.'),Some('.')) = (n,nn) {
                        Ok((ch, Token::Ellipsis))
                    } else {
                        Err("Error parsing ellipses.")
                    }
                },

                _ => Err("Parse Error, unexpected initial token character")
            }
        }
    }
}

fn main() {
    let query: &'static str = "abZc #comment \n  def \"a string\" 123 12.9e24 ... ! [] | 123 wuoah";
    let mut iter = query.chars();
    let mut ch = iter.next();
    loop {
        match next_token(ch, &mut iter) {
            Ok((next_ch,token)) => {
                println!("Token: {:?}", token);
                ch = next_ch;
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