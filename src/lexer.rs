// The shell shall read its input in terms of lines.
// (For details about how the shell reads its input, see the description of sh.)
// The input lines can be of unlimited length. These lines shall be parsed using two major modes:
// ordinary token recognition and processing of here-documents.
//
// When an io_here token has been recognized by the grammar (see Shell Grammar),
// one or more of the subsequent lines immediately following the next NEWLINE token form
// the body of one or more here-documents and shall be parsed according to the rules of Here-Document.
//
// When it is not processing an io_here, the shell shall break its input into tokens by applying the first
// applicable rule below to the next character in its input.
// The token shall be from the current position in the input until a token is delimited according to one
// of the rules below;
// the characters forming the token are exactly those in the input, including any quoting characters.
// If it is indicated that a token is delimited, and no characters have been included in a token, processing shall
// continue until an actual token is delimited.
//
// If the end of input is recognized, the current token (if any) shall be delimited.
//
// If the previous character was used as part of an operator and the current character is not quoted and can be used
// with the previous characters to form an operator, it shall be used as part of that (operator) token.
//
// If the previous character was used as part of an operator and the current character cannot be used with the previous
// characters to form an operator, the operator containing the previous character shall be delimited.
//
// If the current character is <backslash>, single-quote, or double-quote and it is not quoted, it shall affect
// quoting for subsequent characters up to the end of the quoted text. The rules for quoting are as described
// in Quoting . During token recognition no substitutions shall be actually performed, and the result token
// shall contain exactly the characters that appear in the input (except for <newline> joining), unmodified,
// including any embedded or enclosing quotes or substitution operators, between the <quotation-mark> and the
// end of the quoted text. The token shall not be delimited by the end of the quoted field.
//
// If the current character is an unquoted '$' or '`', the shell shall identify the start of any candidates
// for parameter expansion (Parameter Expansion), command substitution (Command Substitution),
// or arithmetic expansion (Arithmetic Expansion) from their introductory unquoted character sequences:
// '$' or "${", "$(" or '`', and "$((", respectively. The shell shall read sufficient input to determine the end
// of the unit to be expanded (as explained in the cited sections). While processing the characters, if instances
// of expansions or quoting are found nested within the substitution, the shell shall recursively process them
// in the manner specified for the construct that is found. The characters found from the beginning of the
// substitution to its end, allowing for any recursion necessary to recognize embedded constructs, shall be
// included unmodified in the result token, including any embedded or enclosing substitution operators or quotes.
// The token shall not be delimited by the end of the substitution.
//
// If the current character is not quoted and can be used as the first character of a new operator,
// the current token (if any) shall be delimited.
// The current character shall be used as the beginning of the next (operator) token.
//
// If the current character is an unquoted <blank>, any token containing the previous character
// is delimited and the current character shall be discarded.
//
// If the previous character was part of a word, the current character shall be appended to that word.
//
// If the current character is a '#', it and all subsequent characters up to, but excluding,
// the next <newline> shall be discarded as a comment.
// The <newline> that ends the line is not considered part of the comment.
//
// The current character is used as the start of a new word.
//
// Once a token is delimited, it is categorized as required by the grammar in Shell Grammar.

// two parsing mode:
// - normal
// - here-document (after an io-here token is found in normal mode)
//
// normal mode rules:
// - if token is empty, skip it
// - if EOF, return current token immediately
// - continue operator if possible
// - if not, return current token and start new token
// - quoting
// - $ and derivatives
// - if operator char, return current token and start operator
// - discard blank
// - continue word
// - comment

macro_rules! push_tok {
    ($vec:ident, $tok:ident) => {{
        if !$tok.is_empty() {
            $vec.push(Token {
                t: match &$tok[..] {
                    "for" => TokenType::For,
                    "if" => TokenType::If,
                    "then" => TokenType::Then,
                    "else" => TokenType::Else,
                    "elif" => TokenType::Elif,
                    "fi" => TokenType::Fi,
                    "do" => TokenType::Do,
                    "done" => TokenType::Done,
                    "case" => TokenType::Case,
                    "esac" => TokenType::Esac,
                    "while" => TokenType::While,
                    "until" => TokenType::Until,
                    "{" => TokenType::Lbrace,
                    "}" => TokenType::Rbrace,
                    "!" => TokenType::Bang,
                    "in" => TokenType::In,
                    _ => TokenType::Word,
                },
                str_val: $tok,
            });
        }
        $tok = String::new();
    }};
    ($vec:ident, $tok:ident, $t:ident) => {{
        if !$tok.is_empty() {
            $vec.push(Token {
                t: TokenType::$t,
                str_val: $tok,
            });
        }
        $tok = String::new();
    }};
}

macro_rules! recognize {
    ($str:ident, $vec:ident, $tok:ident, $s:expr, $len:expr, $t:ident) => {{
        if $str.starts_with($s) {
            push_tok!($vec, $tok);
            $vec.push(Token {
                t: TokenType::$t,
                str_val: $s.to_string(),
            });
            $str = &$str[$len..];
            continue;
        }
    }};
}

pub fn read_here_document(s: &str) -> (String, &str) {
    ("".to_string(), s)
}

pub fn lex(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut cur_token = String::new();
    let mut s = s;
    while !s.is_empty() {
        // quoting
        if s.starts_with('\\') {
            s = &s[1..];
            if !s.starts_with('\n') {
                cur_token.push(s.chars().next().unwrap());
            }
            if !s.is_empty() {
                s = &s[1..];
            }
            continue;
        }
        if s.starts_with('\'') {
            cur_token.push('\'');
            s = &s[1..];
            while !s.is_empty() {
                if s.starts_with('\'') {
                    cur_token.push('\'');
                    s = &s[1..];
                    break;
                }
                cur_token.push(s.chars().next().unwrap());
                s = &s[1..];
            }
            continue;
        }
        if s.starts_with('"') {
            // NO! should be different from 'string'
            cur_token.push('"');
            s = &s[1..];
            while !s.is_empty() {
                if s.starts_with('"') {
                    cur_token.push('"');
                    s = &s[1..];
                    break;
                }
                cur_token.push(s.chars().next().unwrap());
                s = &s[1..];
            }
            continue;
        }

        if s.starts_with('$') {
            s = &s[1..];
            if s.starts_with('\'') || s.starts_with('\"') {
                // TODO
                // Ansi C quoting: $'string' yields string with escaped characters
                // translated string: $"string"
                cur_token.push('"');
                s = &s[1..];
                while !s.is_empty() {
                    if s.starts_with('"') {
                        cur_token.push('"');
                        s = &s[1..];
                        break;
                    }
                    cur_token.push(s.chars().next().unwrap());
                    s = &s[1..];
                }
                continue;
            }
            if s.starts_with("((") {
                // arithmetic expansion
                continue;
            }
            if s.starts_with('(') {
                // subshell
                continue;
            }
            if s.starts_with('{') {
                // parameter expansion
                continue;
            }
            // parameter expansion
        }
        if s.starts_with('`') {
            // subshell
        }

        // prepare operators that can use a IoNumber
        if (s.starts_with('<') || s.starts_with('>'))
            && !cur_token.is_empty()
            && cur_token.chars().all(char::is_numeric)
        {
            push_tok!(tokens, cur_token, IoNumber);
        }

        // operators
        if s.starts_with("<<") {
            push_tok!(tokens, cur_token);
            s = &s[2..];
            if s.starts_with('-') {
                tokens.push(Token {
                    t: TokenType::DLessDash,
                    str_val: "".to_string(),
                });
                s = &s[1..];
            } else {
                tokens.push(Token {
                    t: TokenType::DLess,
                    str_val: "".to_string(),
                });
            }
            let filehere;
            (filehere, s) = read_here_document(s);
            tokens.push(Token {
                t: TokenType::Word,
                str_val: filehere,
            });
            continue;
        }
        recognize!(s, tokens, cur_token, "&&", 2, AndIf);
        recognize!(s, tokens, cur_token, "||", 2, OrIf);
        recognize!(s, tokens, cur_token, ";;", 2, DSemi);
        recognize!(s, tokens, cur_token, "<<-", 3, DLessDash);
        recognize!(s, tokens, cur_token, "<<", 2, DLess);
        recognize!(s, tokens, cur_token, ">>", 2, DGreat);
        recognize!(s, tokens, cur_token, "<&", 2, LessAnd);
        recognize!(s, tokens, cur_token, ">&", 2, GreatAnd);
        recognize!(s, tokens, cur_token, "<>", 2, LessGreat);
        recognize!(s, tokens, cur_token, ">|", 2, Clobber);
        recognize!(s, tokens, cur_token, '&', 1, Async);
        recognize!(s, tokens, cur_token, ';', 1, Semi);
        recognize!(s, tokens, cur_token, '|', 1, Pipe);
        recognize!(s, tokens, cur_token, '(', 1, Lparens);
        recognize!(s, tokens, cur_token, ')', 1, Rparens);
        recognize!(s, tokens, cur_token, '<', 1, Less);
        recognize!(s, tokens, cur_token, '>', 1, Great);
        recognize!(s, tokens, cur_token, '\n', 1, Newline);

        // whitespace
        if s.chars().next().unwrap().is_whitespace() {
            push_tok!(tokens, cur_token);
            s = &s[1..];
            continue;
        }

        // comments
        if s.starts_with('#') {
            push_tok!(tokens, cur_token);
            while !s.starts_with('\n') {
                s = &s[1..];
            }
            s = &s[1..];
            continue;
        }

        // word
        cur_token.push(s.chars().next().unwrap());
        s = &s[1..];
    }
    push_tok!(tokens, cur_token);
    assert!(s.is_empty() && cur_token.is_empty());
    tokens
}

#[derive(Debug)]
pub enum TokenType {
    Word,
    _AssignmentWord,
    _Name,
    Newline,
    IoNumber,
    AndIf,     // &&
    OrIf,      // ||
    DSemi,     // ;;
    DLess,     // <<
    DGreat,    // >>
    LessAnd,   // <&
    GreatAnd,  // >&
    LessGreat, // <>
    DLessDash, // <<-
    Clobber,   // >|
    If,        // if
    Then,      // then
    Else,      // else
    Elif,      // elif
    Fi,        // fi
    Do,        // do
    Done,      // done
    Case,      // case
    Esac,      // esac
    While,     // while
    Until,     // until
    For,       // for
    Lbrace,    // {
    Rbrace,    // }
    Bang,      // !
    In,        // in

    Async,   // &
    Semi,    // ;
    Pipe,    // |
    Lparens, // (
    Rparens, // )
    Less,    // <
    Great,   // >
}

#[derive(Debug)]
pub struct Token {
    pub t: TokenType,
    pub str_val: String,
}
