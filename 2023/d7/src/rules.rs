use std::vec;

#[derive(Default)]
pub struct Rules<'a, T, O>(Vec<Box<dyn Fn(&T) -> O + 'a>>);

impl<'a, T, O> Rules<'a, T, O> {
    pub fn new() -> Self {
        Rules(Vec::new())
    }

    pub fn rule(mut self, f: impl Fn(&T) -> O + 'a) -> Self {
        self.0.push(Box::new(f));
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &Box<dyn Fn(&T) -> O + 'a>> {
        self.0.iter()
    }
}

impl<'a, T, O> Rules<'a, T, Option<O>> {
    pub fn find_unwrap(&self, val: &T) -> O {
        self.iter().filter_map(|f| f(val)).next().unwrap()
    }
}

impl<'a, T, O> IntoIterator for Rules<'a, T, O> {
    type Item = Box<dyn Fn(&T) -> O + 'a>;
    type IntoIter = vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
