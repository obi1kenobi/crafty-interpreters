#![allow(dead_code)]

use std::{cmp::Ordering, fmt::Debug};

#[derive(Debug, Clone)]
pub struct MultiPeekable<I, const N: usize>
where
    I: Iterator,
    I::Item: Default + Debug + Copy,
{
    buffer: CircularBuffer<Option<I::Item>, N>,
    iter: I,
}

impl<I, const N: usize> MultiPeekable<I, N>
where
    I: Iterator,
    I::Item: Default + Debug + Copy,
{
    pub fn new(iter: I) -> Self {
        Self {
            buffer: Default::default(),
            iter,
        }
    }

    pub fn peek(&mut self) -> Option<I::Item> {
        self.peek_nth(0)
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<I::Item> {
        assert!(n < N);

        if self.buffer.len() < n + 1 {
            // n is 0-indexed but length is 1-indexed
            let need_to_push = n + 1 - self.buffer.len();
            for _ in 0..need_to_push {
                self.buffer.push(self.iter.next());
            }
        }

        self.buffer
            .get_nth(n)
            .expect("getting nth element from already-loaded peek buffer")
    }
}

impl<I, const N: usize> Iterator for MultiPeekable<I, N>
where
    I: Iterator,
    I::Item: Default + Debug + Copy,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked_value) = self.buffer.pop() {
            peeked_value
        } else {
            self.iter.next()
        }
    }
}

#[derive(Debug, Clone)]
struct CircularBuffer<T: Default + Debug + Copy, const N: usize> {
    buffer: [T; N],
    start: usize,
    end: usize,
    cycled: bool, // true when start == end indicates a full buffer, false when empty buffer
}

impl<T: Default + Debug + Copy, const N: usize> CircularBuffer<T, N> {
    fn new() -> Self {
        Self {
            buffer: [T::default(); N],
            start: 0,
            end: 0,
            cycled: false,
        }
    }

    #[inline]
    fn next_index(index: usize) -> usize {
        let next_index = index + 1;
        if next_index == N {
            0
        } else {
            next_index
        }
    }

    fn push(&mut self, elem: T) {
        if self.cycled {
            panic!("overflow: attempted to push more than N things into the buffer")
        }

        let next_end = CircularBuffer::<T, N>::next_index(self.end);
        if next_end == self.start {
            self.cycled = true;
        }

        self.buffer[self.end] = elem;
        self.end = next_end;
    }

    fn pop(&mut self) -> Option<T> {
        if !self.cycled && self.start == self.end {
            None
        } else {
            self.cycled = false;
            let elem = self.buffer[self.start];
            self.buffer[self.start] = T::default();
            self.start = CircularBuffer::<T, N>::next_index(self.start);
            Some(elem)
        }
    }

    fn len(&self) -> usize {
        match self.end.cmp(&self.start) {
            Ordering::Less => self.end + N - self.start,
            Ordering::Equal => {
                if self.cycled {
                    N
                } else {
                    0
                }
            }
            Ordering::Greater => self.end - self.start,
        }
    }

    fn get_nth(&mut self, n: usize) -> Option<T> {
        assert!(n < N);
        if self.len() <= n {
            None
        } else {
            let mut index = self.start + n;
            if index >= N {
                index -= N;
            }
            Some(self.buffer[index])
        }
    }
}

impl<T: Default + Debug + Copy, const N: usize> Default for CircularBuffer<T, N> {
    fn default() -> Self {
        Self::new()
    }
}
