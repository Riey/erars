use std::{
    fmt,
    io::{BufReader, Read},
    path::PathBuf,
    sync::Arc,
};

use source_span::{
    fmt::{Formatter, Style},
    DefaultMetrics, Position, SourceBuffer, Span,
};
use utf8_decode::UnsafeDecoder;

#[derive(Clone, Debug)]
pub enum Source {
    File(PathBuf),
    Text(Arc<str>),
}

impl Source {
    pub fn text(text: &str) -> Self {
        Self::Text(text.into())
    }

    pub fn read_str(&self) -> std::io::Result<String> {
        match self {
            Self::File(path) => std::fs::read_to_string(path),
            Self::Text(t) => Ok(t.to_string()),
        }
    }
}

macro_rules! render_source {
    ($decoder:expr, $fmt:expr, $f:expr) => {{
        let buffer = SourceBuffer::new(
            $decoder,
            Position::default(),
            DefaultMetrics::with_tab_stop(4),
        );

        let formatted = $fmt
            .render(
                buffer.iter(),
                buffer.span(),
                &DefaultMetrics::with_tab_stop(4),
            )
            .map_err(|_| fmt::Error)?;

        fmt::Display::fmt(&formatted, $f)?;
    }};
}

impl Source {
    fn render(&self, fmt: &Formatter, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Source::Text(t) => {
                render_source!(
                    UnsafeDecoder::new(t.as_bytes().iter().map(|c| Ok::<u8, std::io::Error>(*c))),
                    fmt,
                    f
                );
            }
            Source::File(path) => {
                let file = BufReader::new(std::fs::File::open(path).map_err(|_| fmt::Error)?);
                render_source!(UnsafeDecoder::new(file.bytes()), fmt, f);
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct SourceLocation {
    pub source: Source,
    pub span: Span,
}

#[derive(Clone)]
pub struct SourceLocationMessage<'a, M: fmt::Display> {
    pub location: &'a SourceLocation,
    pub style: Style,
    pub message: M,
}

impl<'a, M: fmt::Display> SourceLocationMessage<'a, M> {
    pub fn new(location: &'a SourceLocation, style: Style, message: M) -> Self {
        Self {
            location,
            style,
            message,
        }
    }
}

impl<'a, M: fmt::Display> fmt::Display for SourceLocationMessage<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut fmt = source_span::fmt::Formatter::new();
        fmt.add(self.location.span, None, self.style);

        self.location.source.render(&fmt, f)
    }
}
