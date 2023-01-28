use super::{
    ppgroup::PPGroup,
    pptoken::{HChar, PPToken, PPTokens, QChar},
    preprocessor::PPAble,
};

#[derive(Debug, Clone)]
pub enum PPInclude {
    Unparsed(PPTokens),
    HName(Vec<HChar>),
    QName(Vec<QChar>),
    Included(Box<PPGroup>),
}

impl PPInclude {
    pub fn resolve(tokens: PPTokens) -> Self {
        match tokens {
            PPTokens(PPToken::HChars(_, hchars), tail) if tail.is_empty() => {
                PPInclude::HName(hchars)
            }
            PPTokens(PPToken::QChars(_, qchars), tail) if tail.is_empty() => {
                PPInclude::QName(qchars)
            }
            _ => PPInclude::Unparsed(tokens),
        }
    }
}

impl PPAble for PPInclude {
    fn pass(&self, ctx: &super::preprocessor::PPContext) -> Result<Self, crate::error::KrangError> {
        // TODO handle Unparsed
        Ok(self.clone())
    }
}
