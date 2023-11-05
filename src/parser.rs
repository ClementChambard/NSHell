use std::io::Write;

use crate::lexer::*;

#[derive(Default)]
pub struct ProgramOutput {
    pub stdout: String,
    pub code: i32,
    pub success: bool,
}

impl ProgramOutput {
    pub fn combine(&mut self, other: &Self) {
        self.stdout.push_str(&other.stdout);
        self.code = other.code;
        self.success = other.success;
    }
}

#[derive(Copy, Clone, Debug)]
enum Separator {
    Semi,
    Async,
    Newline,
    None,
}

fn parse_separator(input: &[Token], allow_newline: bool) -> Option<(Separator, &[Token])> {
    if input.is_empty() {
        return None;
    }
    if !allow_newline {
        return Some((
            match input[0].t {
                TokenType::Async => Separator::Async,
                TokenType::Semi => Separator::Semi,
                _ => {
                    return None;
                }
            },
            &input[1..],
        ));
    }
    let sep = match input[0].t {
        TokenType::Async => Separator::Async,
        TokenType::Semi => Separator::Semi,
        TokenType::Newline => Separator::Newline,
        _ => {
            return None;
        }
    };
    let input = parse_line_break_opt(&input[1..]);
    Some((sep, input))
}

#[derive(Debug)]
struct ProgramCommand {
    subcommands: Vec<AndOr>,
}

#[derive(Debug)]
pub struct Program {
    commands: Vec<ProgramCommand>,
    // parse: linebreak, opt: programCommand sep by newline tokens
}

fn parse_line_break_opt(input: &[Token]) -> &[Token] {
    let mut input = input;
    while let Some(Token {
        t: TokenType::Newline,
        str_val: _,
    }) = input.first()
    {
        input = &input[1..];
    }
    input
}

fn parse_line_break(input: &[Token], min: usize) -> Result<&[Token], String> {
    let mut seen = 0;
    for t in input {
        if let TokenType::Newline = t.t {
            seen += 1;
        } else if seen < min {
            return Err("Expected newline".to_string());
        } else {
            break;
        }
    }
    Ok(&input[seen..])
}

impl Program {
    pub fn parse(input: &[Token]) -> Result<Self, String> {
        let mut input = parse_line_break_opt(input);
        if input.is_empty() {
            return Ok(Self {
                commands: Vec::new(),
            });
        }
        let fst_command;
        (fst_command, input) = ProgramCommand::parse(input)?;
        let mut commands = vec![fst_command];
        while !input.is_empty() {
            input = parse_line_break(input, 1)?;
            if input.is_empty() {
                break;
            }
            let command;
            (command, input) = ProgramCommand::parse(input)?;
            commands.push(command);
        }
        Ok(Self { commands })
    }

    pub fn exec(&self) -> ProgramOutput {
        let mut output = ProgramOutput::default();
        for command in &self.commands {
            output.combine(&command.exec());
            if !output.success {
                return output;
            }
        }
        output
    }
}

#[derive(Debug)]
struct AndOr {
    sep: Separator,
    pipelines: Vec<PipeSequence>,
}

fn parse_andorcond(input: &[Token]) -> Option<(AndOrCond, &[Token])> {
    if let Some(next) = input.first() {
        let cnd = match next.t {
            TokenType::AndIf => AndOrCond::AndIf,
            TokenType::OrIf => AndOrCond::OrIf,
            _ => return None,
        };
        let input = parse_line_break_opt(&input[1..]);
        Some((cnd, input))
    } else {
        None
    }
}

impl AndOr {
    pub fn parse(input: &[Token]) -> Option<(Self, &[Token])> {
        let (fst_data, mut input) = PipeSequence::parse(input)?;
        let mut data = vec![fst_data];
        while let Some((cnd, input_)) = parse_andorcond(input) {
            input = input_;
            data.last_mut().unwrap().cond = cnd;
            let new_data;
            (new_data, input) = PipeSequence::parse(input).unwrap();
            data.push(new_data);
        }
        Some((
            Self {
                sep: Separator::None,
                pipelines: data,
            },
            input,
        ))
    }

    pub fn exec(&self) -> ProgramOutput {
        // depending on sep, do it async ?
        let mut stdout = String::new();
        let mut code = 0;
        let mut success = true;
        for pipeline in &self.pipelines {
            success = match pipeline.exec() {
                Ok(ChildResult::Output(o)) => {
                    stdout.push_str(&String::from_utf8_lossy(&o.stdout));
                    code = o.status.code().unwrap();
                    o.status.success()
                }
                Ok(ChildResult::Builtin(bi)) => {
                    stdout.push_str(&bi.stdout);
                    code = bi.code;
                    bi.success
                }
                Ok(ChildResult::Exec(_)) => false,
                Err(e) => {
                    stdout.push_str(&e);
                    false
                }
            };
            match pipeline.cond {
                AndOrCond::Always => {}
                AndOrCond::OrIf => {
                    if success {
                        break;
                    }
                }
                AndOrCond::AndIf => {
                    if !success {
                        break;
                    }
                }
            }
        }
        ProgramOutput {
            stdout,
            code,
            success,
        }
    }
}

impl ProgramCommand {
    pub fn parse(input: &[Token]) -> Result<(Self, &[Token]), String> {
        let (fst_data, mut input) =
            AndOr::parse(input).ok_or("At least one command was expected")?;
        let mut data = vec![fst_data];
        while let Some((sep, input_)) = parse_separator(input, false) {
            data.last_mut().unwrap().sep = sep;
            input = input_;
            if let Some((new_data, input_)) = AndOr::parse(input) {
                data.push(new_data);
                input = input_
            } else {
                break;
            }
        }
        Ok((Self { subcommands: data }, input))
    }

    pub fn exec(&self) -> ProgramOutput {
        let mut output = ProgramOutput::default();
        for sub in &self.subcommands {
            output.combine(&sub.exec());
            if !output.success {
                return output;
            }
        }
        output
    }
}

#[derive(Debug)]
pub struct SimpleInstr {
    pub assignments: Vec<String>,
    pub words: Vec<String>,
    pub redirect: Vec<Redirect>,
}
#[derive(Debug)]
struct FuncDefInstr {
    _name: String,
    _body: Box<Instruction>, // can't be Simple or FuncDef
}
#[derive(Debug)]
struct ForClauseInstr {
    _name: String,
    _words: Vec<String>,
    _do_group: Vec<AndOr>,
}
#[derive(Debug)]
struct CaseClauseInstr {}
#[derive(Debug)]
struct IfClauseInstr {
    _cond: Vec<AndOr>,
    _then_bloc: Vec<AndOr>,
    _elsifs: Vec<(Vec<AndOr>, Vec<AndOr>)>,
    _finalelse: Option<Vec<AndOr>>,
}
#[derive(Debug)]
struct WhileClauseInstr {
    _cond: Vec<AndOr>,
    _do_group: Vec<AndOr>,
}
#[derive(Debug)]
struct UntilClauseInstr {
    _cond: Vec<AndOr>,
    _do_group: Vec<AndOr>,
}

#[derive(Debug)]
enum Instruction {
    Simple(SimpleInstr),
    BraceGroup(Vec<AndOr>, Vec<Redirect>),
    Subshell(Vec<AndOr>, Vec<Redirect>),
    ForClause(ForClauseInstr, Vec<Redirect>),
    CaseClause(CaseClauseInstr, Vec<Redirect>),
    IfClause(IfClauseInstr, Vec<Redirect>),
    WhileClause(WhileClauseInstr, Vec<Redirect>),
    UntilClause(UntilClauseInstr, Vec<Redirect>),
    FuncDef(FuncDefInstr),
}

#[derive(Debug)]
pub struct Redirect {
    _io_number: Option<String>,
    _op: String,
    _name: String,
}

impl Redirect {
    pub fn parse(input: &[Token]) -> Option<(Self, &[Token])> {
        let mut input = input;
        let mut io_number = None;
        if let Some(Token {
            t: TokenType::IoNumber,
            str_val,
        }) = input.first()
        {
            io_number = Some(str_val.clone());
            input = &input[1..];
        }
        let (op, name) = match input.first()?.t {
            TokenType::Less
            | TokenType::LessAnd
            | TokenType::Great
            | TokenType::GreatAnd
            | TokenType::DGreat
            | TokenType::LessGreat
            | TokenType::Clobber => {
                let op = input.first()?.str_val.clone();
                input = &input[1..];
                if let TokenType::Word = input.first()?.t {
                } else {
                    return None;
                    // panic!();
                }
                let name = input.first()?.str_val.clone();
                // rule 2
                input = &input[1..];
                (op, name)
            }
            TokenType::DLess | TokenType::DLessDash => {
                let op = input.first()?.str_val.clone();
                input = &input[1..];
                if let TokenType::Word = input.first()?.t {
                } else {
                    panic!();
                }
                let name = input.first()?.str_val.clone();
                // rule 3
                input = &input[1..];
                (op, name)
            }
            _ => return None,
        };
        Some((
            Self {
                _io_number: io_number,
                _op: op,
                _name: name,
            },
            input,
        ))
    }
}

fn parse_redirect_list(input: &[Token]) -> (Vec<Redirect>, &[Token]) {
    let mut input = input;
    let mut data = Vec::new();
    while let Some((red, toks)) = Redirect::parse(input) {
        input = toks;
        data.push(red);
    }
    (data, input)
}

pub enum ChildResult {
    Builtin(crate::builtins::Output),
    Exec(std::process::Child),
    Output(std::process::Output),
}

impl ChildResult {
    pub fn _stdout_str(&self) -> String {
        match self {
            Self::Builtin(o) => o.stdout.clone(),
            Self::Output(c) => String::from_utf8_lossy(&c.stdout).into(),
            _ => "".to_string(),
        }
    }
}

pub enum StdInOfInstruction {
    String(String),
    Stdio(std::process::Stdio),
    None,
}

impl Instruction {
    pub fn exec(&self, stdin: StdInOfInstruction) -> Result<ChildResult, String> {
        match self {
            Instruction::Simple(si) => {
                // check builtin first:
                if let Some(output) = crate::builtins::check(si) {
                    return Ok(ChildResult::Builtin(output));
                }
                let mut builder =
                    std::process::Command::new(si.words.first().ok_or("Empty command\n")?);
                for w in &si.words[1..] {
                    builder.arg(w);
                }
                if let StdInOfInstruction::Stdio(stdin) = stdin {
                    builder.stdin(stdin);
                } else if let StdInOfInstruction::String(s) = stdin {
                    let mut child = builder
                        .stdin(std::process::Stdio::piped())
                        .stdout(std::process::Stdio::piped())
                        .spawn()
                        .map_err(|e| format!("Failed to execute command: {e}\n"))?;
                    if let Some(ref mut stdin) = child.stdin {
                        stdin
                            .write_all(s.as_bytes())
                            .map_err(|e| format!("Failed to write to stdin: {e}"))?;
                    } else {
                        return Err("Failed to open stdin".to_string());
                    }
                    return Ok(ChildResult::Exec(child));
                }
                Ok(ChildResult::Exec(
                    builder
                        .stdout(std::process::Stdio::piped())
                        .spawn()
                        .map_err(|e| format!("Failed to execute command: {e}\n"))?,
                ))
            }
            _ => {
                unimplemented!();
            }
        }
    }

    pub fn parse_compound(input: &[Token]) -> Option<(Self, &[Token])> {
        match input.first()?.t {
            TokenType::Lbrace => {
                let (body, input) = parse_compound_list(&input[1..]).ok()?;
                if let TokenType::Rbrace = input.first()?.t {
                    let (red, input) = parse_redirect_list(&input[1..]);
                    Some((Self::BraceGroup(body, red), input))
                } else {
                    // panic!();
                    None
                }
            }
            TokenType::Lparens => {
                let (body, input) = parse_compound_list(&input[1..]).ok()?;
                if let TokenType::Rparens = input.first()?.t {
                    let (red, input) = parse_redirect_list(&input[1..]);
                    Some((Self::Subshell(body, red), input))
                } else {
                    // panic!();
                    None
                }
            }
            TokenType::For => {
                let (body, input) = Self::parse_for(&input[1..]).ok()?;
                let (red, input) = parse_redirect_list(input);
                Some((Self::ForClause(body, red), input))
            }
            TokenType::Case => {
                let (body, input) = Self::parse_case(&input[1..]);
                let (red, input) = parse_redirect_list(input);
                Some((Self::CaseClause(body, red), input))
            }
            TokenType::If => {
                let (body, input) = Self::parse_if(&input[1..]).ok()?;
                let (red, input) = parse_redirect_list(input);
                Some((Self::IfClause(body, red), input))
            }
            TokenType::While => {
                let (body, input) = Self::parse_while(&input[1..]).ok()?;
                let (red, input) = parse_redirect_list(input);
                Some((Self::WhileClause(body, red), input))
            }
            TokenType::Until => {
                let (body, input) = Self::parse_until(&input[1..]).ok()?;
                let (red, input) = parse_redirect_list(input);
                Some((Self::UntilClause(body, red), input))
            }
            _ => None,
        }
    }
    pub fn parse(input: &[Token]) -> Option<(Self, &[Token])> {
        if let Some(out) = Self::parse_compound(input) {
            Some(out)
        } else if let TokenType::_Name = input.first()?.t {
            let name = input.first()?.str_val.clone();
            // apply rule 8
            let mut input = &input[1..];
            if let TokenType::Lparens = input.first()?.t {
            } else {
                // panic!()
                return None;
            }
            input = &input[1..];
            if let TokenType::Rparens = input.first()?.t {
            } else {
                // panic!()
                return None;
            }
            let body;
            (body, input) = Self::parse_compound(input)?;
            // apply rule 9
            Some((
                Self::FuncDef(FuncDefInstr {
                    _name: name,
                    _body: Box::new(body),
                }),
                input,
            ))
        } else {
            Self::parse_simple(input)
        }
    }

    pub fn parse_simple(input: &[Token]) -> Option<(Self, &[Token])> {
        let mut redirect = Vec::new();
        let mut assignments = Vec::new();
        let mut words = Vec::new();
        let mut input = input;
        let mut first_word_found = false;
        // let mut has_prefix = false;
        while let Some(tok) = input.first() {
            match tok.t {
                TokenType::_AssignmentWord => {
                    if first_word_found {
                        // panic!()
                        return None;
                    }
                    assignments.push(tok.str_val.clone());
                    // has_prefix = true;
                }
                TokenType::Word => {
                    if words.is_empty() {
                        // if has_prefix {
                        //     // rule 7b
                        // } else {
                        //     // rule 7a
                        // }
                        first_word_found = true;
                    }
                    words.push(tok.str_val.clone());
                }
                _ => {
                    if let Some((red, input_)) = Redirect::parse(input) {
                        redirect.push(red);
                        input = input_;
                        // if words.is_empty() {
                        //     has_prefix = true;
                        // }
                        continue;
                    } else {
                        break;
                    }
                }
            }
            input = &input[1..];
        }
        Some((
            Self::Simple(SimpleInstr {
                assignments,
                redirect,
                words,
            }),
            input,
        ))
    }

    pub fn parse_for(input: &[Token]) -> Result<(ForClauseInstr, &[Token]), String> {
        let name = input.first().ok_or("Expected name token")?; // apply rule 5
        let mut input = parse_line_break_opt(&input[1..]);
        let mut wordlist = Vec::new();
        if let TokenType::In = input.first().ok_or("Expected token after 'for'")?.t {
            // apply rule 6
            input = &input[1..];
            while let Some(tok) = input.first() {
                match &tok.t {
                    TokenType::Word => {
                        wordlist.push(tok.str_val.clone());
                    }
                    TokenType::Newline => break,
                    TokenType::Semi => {
                        input = &input[1..];
                        break;
                    }
                    _ => return Err(format!("Invalid token type {:?}", tok.t)),
                }
                input = &input[1..];
            }
            input = parse_line_break_opt(&input[1..]);
        } else if let TokenType::Semi = input.first().ok_or("Expected token")?.t {
            input = parse_line_break_opt(&input[1..]);
        }
        if let TokenType::Do = input.first().ok_or("Expected token 'do'")?.t {
        } else {
            return Err("Expected token 'do'".to_string());
        }
        let body; // rule 6
        (body, input) = parse_compound_list(&input[1..])?;
        if let TokenType::Done = input.first().ok_or("Expected token 'done'")?.t {
        } else {
            return Err("Expected token 'done'".to_string());
        }
        Ok((
            ForClauseInstr {
                _name: name.str_val.clone(),
                _do_group: body,
                _words: wordlist,
            },
            &input[1..],
        ))
    }

    pub fn parse_case(_input: &[Token]) -> (CaseClauseInstr, &[Token]) {
        unimplemented!();

        /*
        in               : In                       /* Apply rule 6 */
                         ;
        case_clause      : Case WORD linebreak in linebreak case_list    Esac
                         | Case WORD linebreak in linebreak case_list_ns Esac
                         | Case WORD linebreak in linebreak              Esac
                         ;
        case_list_ns     : case_list case_item_ns
                         |           case_item_ns
                         ;
        case_list        : case_list case_item
                         |           case_item
                         ;
        case_item_ns     :     pattern ')' linebreak
                         |     pattern ')' compound_list
                         | '(' pattern ')' linebreak
                         | '(' pattern ')' compound_list
                         ;
        case_item        :     pattern ')' linebreak     DSEMI linebreak
                         |     pattern ')' compound_list DSEMI linebreak
                         | '(' pattern ')' linebreak     DSEMI linebreak
                         | '(' pattern ')' compound_list DSEMI linebreak
                         ;
        pattern          :             WORD         /* Apply rule 4 */
                         | pattern '|' WORD         /* Do not apply rule 4 */
                         ;
        */
    }

    pub fn parse_if(input: &[Token]) -> Result<(IfClauseInstr, &[Token]), String> {
        let (cond, mut input) = parse_compound_list(input)?;
        if let TokenType::Then = input.first().ok_or("Expected token 'then'")?.t {
        } else {
            return Err("Expected token 'then'".to_string());
        }
        let then_bloc;
        (then_bloc, input) = parse_compound_list(&input[1..])?;
        let mut elsifs = Vec::new();
        let mut finalelse = None;
        while let Some(t) = input.first() {
            match t.t {
                TokenType::Fi => {
                    break;
                }
                TokenType::Else => {
                    let finalelse_some;
                    (finalelse_some, input) = parse_compound_list(&input[1..])?;
                    print!("{:?}", finalelse_some);
                    finalelse = Some(finalelse_some);
                    if let TokenType::Fi = input.first().ok_or("Expected token 'fi'")?.t {
                    } else {
                        return Err("Expected token 'fi'".to_string());
                    }
                }
                TokenType::Elif => {
                    let elif_cond;
                    (elif_cond, input) = parse_compound_list(&input[1..])?;
                    if let TokenType::Then = input.first().ok_or("Expected token 'then'")?.t {
                    } else {
                        return Err("Expected token 'then'".to_string());
                    }
                    let elif_then;
                    (elif_then, input) = parse_compound_list(&input[1..])?;
                    elsifs.push((elif_cond, elif_then));
                }
                _ => return Err("Expected one of 'fi', 'else' or 'elif'".to_string()),
            }
        }
        Ok((
            IfClauseInstr {
                _cond: cond,
                _then_bloc: then_bloc,
                _elsifs: elsifs,
                _finalelse: finalelse,
            },
            &input[1..],
        ))
    }

    pub fn parse_while(input: &[Token]) -> Result<(WhileClauseInstr, &[Token]), String> {
        let (cond, mut input) = parse_compound_list(input)?;
        if let TokenType::Do = input.first().ok_or("Expected token 'do'")?.t {
        } else {
            return Err("Expected token 'do'".to_string());
        }
        let body; // rule 6
        (body, input) = parse_compound_list(&input[1..])?;
        if let TokenType::Done = input.first().ok_or("Expected token 'done'")?.t {
        } else {
            return Err("Expected token 'done'".to_string());
        }
        Ok((
            WhileClauseInstr {
                _cond: cond,
                _do_group: body,
            },
            &input[1..],
        ))
    }

    pub fn parse_until(input: &[Token]) -> Result<(UntilClauseInstr, &[Token]), String> {
        let (cond, mut input) = parse_compound_list(input)?;
        if let TokenType::Do = input.first().ok_or("Expected token 'do'")?.t {
        } else {
            return Err("Expected token 'do'".to_string());
        }
        let body; // rule 6
        (body, input) = parse_compound_list(&input[1..])?;
        if let TokenType::Done = input.first().ok_or("Expected token 'done'")?.t {
        } else {
            return Err("Expected token 'done'".to_string());
        }
        Ok((
            UntilClauseInstr {
                _cond: cond,
                _do_group: body,
            },
            &input[1..],
        ))
    }
}

#[derive(Copy, Clone, Debug)]
enum AndOrCond {
    Always,
    AndIf,
    OrIf,
}

#[derive(Debug)]
struct PipeSequence {
    _bang: bool,
    instrs: Vec<Instruction>,
    cond: AndOrCond,
}

impl PipeSequence {
    pub fn parse(input: &[Token]) -> Option<(Self, &[Token])> {
        let mut bang = false;
        let mut input = input;
        if let TokenType::Bang = input.first()?.t {
            bang = true;
            input = &input[1..];
        }
        let fst_command;
        (fst_command, input) = Instruction::parse(input)?;
        let mut data = vec![fst_command];
        while let Some(Token {
            t: TokenType::Pipe,
            str_val: _,
        }) = input.first()
        {
            input = parse_line_break_opt(&input[1..]);
            let next_data;
            (next_data, input) = Instruction::parse(input)?;
            data.push(next_data);
        }
        Some((
            Self {
                _bang: bang,
                instrs: data,
                cond: AndOrCond::Always,
            },
            input,
        ))
    }

    pub fn exec(&self) -> Result<ChildResult, String> {
        let mut last_command = self.instrs[0].exec(StdInOfInstruction::None)?;
        for c in &self.instrs[1..] {
            let old_last_command = last_command;
            last_command = c.exec(match old_last_command {
                ChildResult::Exec(lc) => StdInOfInstruction::Stdio(std::process::Stdio::from(
                    lc.stdout.ok_or("Unable to get stdout of process")?,
                )),
                ChildResult::Output(_) => StdInOfInstruction::None,
                ChildResult::Builtin(bi) => StdInOfInstruction::String(bi.stdout),
            })?;
        }
        Ok(match last_command {
            ChildResult::Exec(lc) => ChildResult::Output(
                lc.wait_with_output()
                    .map_err(|e| format!("Io error: {e}\n"))?,
            ),
            ChildResult::Output(o) => ChildResult::Output(o),
            ChildResult::Builtin(bi) => ChildResult::Builtin(bi),
        })
    }
}

fn parse_compound_list(input: &[Token]) -> Result<(Vec<AndOr>, &[Token]), String> {
    let (fst_data, mut input) =
        AndOr::parse(parse_line_break_opt(input)).ok_or("Expected subcommand")?;
    let mut data = vec![fst_data];
    while let Some((sep, input_)) = parse_separator(input, false) {
        data.last_mut().unwrap().sep = sep;
        input = input_;
        if let Some((new_data, input_)) = AndOr::parse(input) {
            data.push(new_data);
            input = input_
        } else {
            break;
        }
    }
    Ok((data, input))
}
