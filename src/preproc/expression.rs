use super::pptoken::PPTokens;

#[derive(Debug, Clone)]
pub enum PPExpression {
    Unparsed(PPTokens),
}
