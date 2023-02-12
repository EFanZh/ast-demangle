pub trait Find {
    type Item;

    fn find(&self, pattern: impl FnMut(Self::Item) -> bool) -> usize;
}

impl Find for &str {
    type Item = char;

    fn find(&self, pattern: impl FnMut(Self::Item) -> bool) -> usize {
        str::find(self, pattern).unwrap_or(self.len())
    }
}

pub trait SplitAt: Sized {
    type Prefix;

    fn split_at(self, index: usize) -> Option<(Self::Prefix, Self)>;
}

impl SplitAt for &str {
    type Prefix = Self;

    fn split_at(self, index: usize) -> Option<(Self::Prefix, Self)> {
        self.is_char_boundary(index).then(|| self.split_at(index))
    }
}

pub trait StripPrefix<P>: Sized {
    type Prefix;

    fn strip_prefix(self, prefix: P) -> Option<(Self::Prefix, Self)>;
}

impl StripPrefix<char> for &str {
    type Prefix = Self;

    fn strip_prefix(self, prefix: char) -> Option<(Self::Prefix, Self)> {
        self.strip_prefix(prefix)
            .map(|rest| (&self[..self.len() - rest.len()], rest))
    }
}
