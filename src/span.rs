

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}


impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn from(start: &Span, end: &Span) -> Span {
        Span {
            start: start.start,
            end: end.end,
        }
    }

    pub fn empty() -> Span {
        Span { start: 0, end: 0 }
    }

    // Get the data between the start and end of the span
    pub fn data<'a>(&self, data: &'a str) -> &'a str {
        &data[self.start..self.end]
    }




}
