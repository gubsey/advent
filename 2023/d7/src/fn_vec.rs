use std::vec;

type BoxFn<'a, I, O> = Box<dyn Fn(&I) -> O + 'a>;

#[derive(Default)]
pub struct FnVec<'a, I, O>(Vec<BoxFn<'a, I, O>>);

impl<'a, I, O> FnVec<'a, I, O> {
    pub fn new() -> Self {
        FnVec(Vec::new())
    }

    pub fn rule(mut self, f: impl Fn(&I) -> O + 'a) -> Self {
        self.0.push(Box::new(f));
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &Box<dyn Fn(&I) -> O + 'a>> {
        self.0.iter()
    }
}

impl<'a, I, O> FnVec<'a, I, Option<O>> {
    pub fn find_unwrap(&self, val: &I) -> O {
        self.iter().find_map(|f| f(val)).unwrap()
    }
}

impl<'a, T, O> IntoIterator for FnVec<'a, T, O> {
    type Item = Box<dyn Fn(&T) -> O + 'a>;
    type IntoIter = vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
