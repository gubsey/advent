use Valid::*;

pub trait Parser<T> {
    fn parse(self, s: &str) -> Valid<T>;
}

pub struct Then<PA, PB>(PA, PB);

impl<A, B, PA: Parser<A>, PB: Parser<B>> Parser<(A, B)> for Then<PA, PB> {
    fn parse(self, s: &str) -> Valid<(A, B)> {
        match self.0.parse(s) {
            Yes(xs, a) => match self.1.parse(&xs) {
                Yes(xs, b) => Yes(xs, (a, b)),
                No => No,
            },
            No => No,
        }
    }
}

pub enum Valid<T> {
    Yes(String, T),
    No,
}

impl<T> Valid<T> {
    pub fn is_yes(&self) -> bool {
        matches!(self, Yes(_, _))
    }

    pub fn is_no(&self) -> bool {
        matches!(self, No)
    }

    pub fn opt_str(self) -> Option<String> {
        match self {
            Yes(s, _) => Some(s),
            _ => None,
        }
    }

    pub fn opt_t(self) -> Option<T> {
        match self {
            Yes(_, t) => Some(t),
            _ => None,
        }
    }

    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> Valid<O> {
        match self {
            Yes(s, x) => Yes(s, f(x)),
            No => No,
        }
    }

    pub fn then<O>(self, p: impl Parser<O>) -> Valid<(T, O)> {
        match self {
            Yes(xs, a) => p.parse(&xs).map(|b| (a, b)),
            No => No,
        }
    }
}

pub struct Any;

impl Parser<char> for Any {
    fn parse(self, s: &str) -> Valid<char> {
        let mut i = s.chars();
        match i.next() {
            None => No,
            Some(c) => Yes(s[1..].to_string(), c),
        }
    }
}

pub struct Eof;

impl Parser<()> for Eof {
    fn parse(self, s: &str) -> Valid<()> {
        if s.is_empty() {
            Yes(String::new(), ())
        } else {
            No
        }
    }
}

pub struct Char(char);

impl Parser<char> for Char {
    fn parse(self, s: &str) -> Valid<char> {
        let mut i = s.chars();
        match i.next() {
            Some(c) if c == self.0 => Yes(s[1..].to_string(), c),
            _ => No,
        }
    }
}

pub struct Digit;

impl Parser<char> for Digit {
    fn parse(self, s: &str) -> Valid<char> {
        Satisfy(char::is_numeric).parse(s)
    }
}

pub struct Satisfy<F: Fn(char) -> bool>(F);

impl<F: Fn(char) -> bool> Parser<char> for Satisfy<F> {
    fn parse(self, s: &str) -> Valid<char> {
        let mut i = s.chars();
        if let Some(c) = i.next().filter(|c| (self.0)(*c)) {
            Yes(s[1..].to_string(), c)
        } else {
            No
        }
    }
}

pub struct MatchStr(String);

impl Parser<String> for MatchStr {
    fn parse(self, s: &str) -> Valid<String> {
        self.0.chars().map(Char).parse(s).map(String::from_iter)
    }
}

impl<I, P, T> Parser<Vec<T>> for I
where
    I: IntoIterator<Item = P>,
    P: Parser<T>,
{
    fn parse(self, s: &str) -> Valid<Vec<T>> {
        let mut aci = Vec::new();
        let mut s = s;
        for p in self {
            match p.parse(s) {
                Yes(_, x) => {
                    s = &s[1..];
                    aci.push(x)
                }
                No => return No,
            }
        }
        Yes(s.to_string(), aci)
    }
}

#[test]
fn match_str() {
    assert!(MatchStr("tyler".to_string()).parse("tyler time").is_yes());
    assert!(MatchStr("tyler".to_string()).parse("tyle time").is_no());
}
