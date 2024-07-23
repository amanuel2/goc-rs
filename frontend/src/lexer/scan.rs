use std::fs;


pub struct Lexer<'a> {
    input: String,
    idx: usize,
    ch: char,
    line: usize,
    file_name: &'a str,
    is_file: bool,
    err: u64
}


impl<'a> Lexer<'a> {
    fn new(_path: &'static str) -> Option<Lexer> {
        // let res = fs::read_to_string(path);

        // match res {
        //     Some(str) => {
        //         input: str,
        //         idx: 0,
        //         ch: str<
        //             :
        //
        //     }
        //     None => None
        // }
        None
    }
}

