use std::fmt;
use std::io::{self, ErrorKind};

pub struct BoundedWriter<T> {
    inner: T,
    capacity: usize,
}

impl<T> BoundedWriter<T> {
    pub fn new(inner: T, capacity: usize) -> Self {
        Self { inner, capacity }
    }

    pub fn inner(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn into_inner(self) -> T {
        self.inner
    }
}

impl<T> fmt::Write for BoundedWriter<T>
where
    T: fmt::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let s_len = s.len();

        if s_len <= self.capacity {
            self.inner.write_str(s)?;
            self.capacity -= s_len;

            Ok(())
        } else {
            Err(fmt::Error)
        }
    }
}

impl<T> io::Write for BoundedWriter<T>
where
    T: io::Write,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let buf_len = buf.len();

        if buf_len <= self.capacity {
            let length = self.inner.write(buf)?;

            self.capacity -= length;

            Ok(length)
        } else {
            Err(ErrorKind::Other.into())
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
