pub struct Output {
    pub stdout: String,
    pub code: i32,
    pub success: bool,
}

fn cd_builtin(args: &[String]) -> Output {
    lazy_static! {
        static ref CD_DASH_PATH: std::sync::Mutex<std::path::PathBuf> =
            std::sync::Mutex::new(std::env::current_dir().unwrap());
    }

    if args.len() > 1 {
        return Output {
            stdout: "cd: too many arguments\n".to_string(),
            code: 1,
            success: false,
        };
    }

    let absolute_path = if args.is_empty() || args[1] == "~" {
        dirs::home_dir().unwrap()
    } else if args[0] == "-" {
        CD_DASH_PATH.lock().unwrap().clone()
    } else {
        let dir_path_str = &args[0];
        let ap = if dir_path_str.starts_with("~/") {
            let rest_path = dir_path_str.trim_start_matches("~/");
            dirs::home_dir().unwrap().join(rest_path)
        } else {
            std::path::PathBuf::from(dir_path_str)
        };
        match ap.canonicalize() {
            Ok(p) => p,
            Err(e) => {
                return Output {
                    stdout: format!("cd: {}", e),
                    code: 1,
                    success: false,
                };
            }
        }
    };

    *CD_DASH_PATH.lock().unwrap() = std::env::current_dir().unwrap();

    match std::env::set_current_dir(absolute_path) {
        Ok(_) => Output {
            stdout: "".to_string(),
            code: 0,
            success: true,
        },
        Err(e) => Output {
            stdout: format!("cd: {}", e),
            code: 1,
            success: false,
        },
    }
}

fn exit_builtin(/* in bash, can have a num as arg for exit code of the shell */) -> Output {
    *crate::SHOULD_EXIT.lock().unwrap() = true;
    Output {
        stdout: "".to_string(),
        code: 0,
        success: false,
    }
}

pub fn check(cmd: &crate::parser::SimpleInstr) -> Option<Output> {
    match &cmd.words.first()?[..] {
        "cd" => Some(cd_builtin(&cmd.words[1..])),
        "exit" => Some(exit_builtin()),
        _ => None,
    }
}
