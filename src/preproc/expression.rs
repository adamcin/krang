use crate::error::KrangError;

use super::{
    id::Id,
    ppcontext::PPContext,
    pptoken::{PPToken, PPTokens},
};

/// constant-expression:
///     conditional-expression
///
/// conditional-expression:
///     logical-OR-expression
///     logical-OR-expression ? expression : conditional-expression
///
/// logical-OR-expression:
///     logical-AND-expression
///     logical-OR-expression || logical-AND-expression
///
/// logical-AND-expression:
///     inclusive-OR-expression
///     logical-AND-expression && inclusive-OR-expression
///
/// inclusive-OR-expression:
///     exclusive-OR-expression
///     inclusive-OR-expression | exclusive-OR-expression
///
/// exclusive-OR-expression:
///     AND-expression
///     exclusive-OR-expression ^ AND-expression
///
/// AND-expression:
///     equality-expression
///     AND-expression & equality-expression
///
/// equality-expression:
///     relational-expression
///     equality-expression == relational-expression
///     equality-expression != relational-expression
///
/// relational-expression:
///     shift-expression
///     relational-expression < shift-expression
///     relational-expression > shift-expression
///     relational-expression <= shift-expression
///     relational-expression >= shift-expression
///
/// shift-expression:
///     additive-expression
///     shift-expression << additive-expression
///     shift-expression >> additive-expression
///
/// additive-expression:
///     multiplicative-expression
///     additive-expression + multiplicative-expression
///     additive-expression - multiplicative-expression
///
/// multiplicative-expression:
///     cast-expression
///     multiplicative-expression * cast-expression
///     multiplicative-expression / cast-expression
///     multiplicative-expression % cast-expression
///
/// cast-expression:
///     unary-expression
///     ( type-name ) cast-expression
///
/// unary-expression:
///     postfix-expression
///     ++ unary-expression
///     - unary-expression
///     unary-operator cast-expression
///     sizeof unary-expression
///     sizeof ( type-name )
///     _Alignof ( type-name )
///
/// unary-operator: one of
///     & * + - ˜ !
///
/// postfix-expression:
///     primary-expression
///     postfix-expression [ expression ]
///     postfix-expression ( argument-expression-listopt )
///     postfix-expression . identifier
///     postfix-expression -> identifier
///     postfix-expression ++
///     postfix-expression -
///     ( type-name ) { initializer-list }
///     ( type-name ) { initializer-list , }
///
/// argument-expression-list:
///     assignment-expression
///     argument-expression-list , assignment-expression
///
/// primary-expression:
///     identifier
///     constant
///     string-literal
///     ( expression )
///     generic-selection
///
/// generic-selection:
///     _Generic ( assignment-expression , generic-assoc-list )
///
/// generic-assoc-list:
///     generic-association
///     generic-assoc-list , generic-association
///
/// generic-association:
///     type-name : assignment-expression
///     default : assignment-expression
///
/// assignment-expression:
///     conditional-expression
///     unary-expression assignment-operator assignment-expression
///
/// assignment-operator: one of
///     = *= /= %= += -= <<= >>= &= ^= |=
///
/// expression:
///     assignment-expression
///     expression , assignment-expression
///
/// pp-number:
///     digit
///     . digit
///     pp-number digit
///     pp-number identifier-nondigit
///     pp-number e sign
///     pp-number E sign
///     pp-number p sign
///     pp-number P sign
///     pp-number .
///
/// A constant expression can be evaluated during translation rather than runtime, and accordingly
/// may be used in any place that a constant may be.
///
/// Constraints
///
/// Constant expressions shall not contain assignment, increment, decrement, function-call, or comma
/// operators, except when they are contained within a subexpression that is not evaluated.
///
/// Each constant expression shall evaluate to a constant that is in the range of representable values for
/// its type.
///
/// Semantics
///
/// An expression that evaluates to a constant is required in several contexts. If a floating expression
/// is evaluated in the translation environment, the arithmetic range and precision shall be at least as
/// great as if the expression were being evaluated in the execution environment.
///
#[derive(Debug, Clone)]
pub enum PPExpression {
    Unparsed(Vec<PPToken>),
}

impl PPExpression {
    pub fn expand(&self, ctx: &PPContext) -> Result<Option<Self>, String> {
        match self {
            Self::Unparsed(tokens) => {
                let ids: Vec<_> = tokens
                    .to_vec()
                    .iter()
                    .filter_map(|token| match token {
                        PPToken::Id(pos, id) => Some(id.clone()),
                        _ => None,
                    })
                    .collect();
                if ids.is_empty() {
                    Ok(None)
                } else {
                    Ok(None)
                    //match parser.parse()
                }
            }
        }
    }
}

/// primary-expression:
///     identifier
///     constant
///     string-literal
///     ( expression )
///     generic-selection
///
/// pp-constant
///     pp-number
///     character-constant
///
/// constant:
///     integer-constant
///     floating-constant
///     enumeration-constant
///     character-constant
#[derive(Debug, Clone)]
pub enum PPPrimaryExpr {
    Id(Id),
}

#[derive(Debug, Clone)]
pub enum PPConst {}

/// unary-operator: one of
///     & * + - ˜ !
pub enum PPUnaryOp {}
