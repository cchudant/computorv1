use std::env;
use std::iter::Peekable;
use std::fmt::{self, Display};
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum TokenType {
    Num,
    Var,
    Exp,
    Mul,
    Equal,
    Plus,
    Minus,
    Unknown,
}

impl TokenType {
    fn name(self) -> &'static str {
        match self {
            TokenType::Num => "litteral",
            TokenType::Var => "variable",
            TokenType::Exp => "'^'",
            TokenType::Mul => "'*'",
            TokenType::Equal => "'='",
            TokenType::Plus => "'+'",
            TokenType::Minus => "'-'",
            TokenType::Unknown => "unknown",
        }
    }
}

#[derive(Debug)]
struct Token {
    ty: TokenType,
    value: String,
}

#[derive(Debug)]
struct TokenStream<I: Iterator<Item=char>> {
    input: Peekable<I>,
}

impl<I: Iterator<Item=char>> TokenStream<I> {
    fn new(input: I) -> Self {
        Self {
            input: input.peekable()
        }
    }

    fn read_while(&mut self, f: impl Fn(char) -> bool) -> String {
        let mut s = String::new();
        while let Some(c) = self.input.peek().filter(|c| f(**c)) {
            s.push(*c);
            self.input.next();
        }
        s
    }

    fn read_next(&mut self) -> Option<Token> {
        self.read_while(|c| c == '\t' || c == ' ');

        let c = self.input.peek().copied();
        match c {
            Some(c) if "0123456789".contains(c) => {
                Some(Token {
                    ty: TokenType::Num,
                    value: self.read_while(|c| "0123456789.".contains(c)),
                })
            },
            Some(c @ 'X') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Var,
                    value: String::from(c),
                })
            },
            Some(c @ '^') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Exp,
                    value: String::from(c),
                })
            },
            Some(c @ '=') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Equal,
                    value: String::from(c),
                })
            },
            Some(c @ '*') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Mul,
                    value: String::from(c),
                })
            },
            Some(c @ '+') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Plus,
                    value: String::from(c),
                })
            },
            Some(c @ '-') => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Minus,
                    value: String::from(c),
                })
            },
            Some(c) => {
                self.input.next();
                Some(Token {
                    ty: TokenType::Unknown,
                    value: String::from(c),
                })
            },
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
struct Term {
    a: f64,
    p: i64,
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.p == 0 {
            write!(f, "{}", self.a)?;
        } else if self.p == 1 {
            write!(f, "{} * X", self.a)?;
        } else {
            write!(f, "{} * X^{}", self.a, self.p)?;
        }

        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
struct Equation {
    left: Vec<Term>,
    right: Vec<Term>,
}

impl Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        fn fmt_one_side(f: &mut fmt::Formatter<'_>, side: &Vec<Term>) -> fmt::Result {
            for (i, el) in side.iter().enumerate() {
                let mut a = el.a;
                if i > 0 {
                    if a < 0.0 {
                        write!(f, " - ")?;
                        a *= -1.0;
                    } else {
                        write!(f, " + ")?;
                    }
                }

                if el.p == 0 {
                    write!(f, "{}", a)?;
                } else if el.p == 1 {
                    write!(f, "{} * X", a)?;
                } else {
                    write!(f, "{} * X^{}", a, el.p)?;
                }
            }

            Ok(())
        }

        fmt_one_side(f, &self.left)?;
        write!(f, " = ")?;
        fmt_one_side(f, &self.right)?;

        Ok(())
    }
}

#[derive(Debug)]
struct Parser<'a, I: Iterator<Item=char>> {
    tokens: &'a mut TokenStream<I>,
    right_side: bool,
    err: bool
}

impl<'a, I: Iterator<Item=char>> Parser<'a, I> {
    fn new(tokens: &'a mut TokenStream<I>) -> Self {
        Self { tokens, right_side: false, err: false }
    }

    fn error(&mut self, msg: impl AsRef<str>) {
        self.err = true;
        eprintln!("{}", msg.as_ref());
    }

    fn expect_token(&mut self, t: &Option<Token>, ty: TokenType) -> bool {
        match t {
            Some(ty2) if ty2.ty == ty => true,
            _ => {
                self.error(format!("Expected token {}", ty.name()));
                false
            }
        }
    }

    fn parse(&mut self) -> Option<Equation> {
        let mut equation = Equation::default();

        let mut token_opt = self.tokens.read_next();

        while let Some(ref token) = token_opt {
            match token.ty {
                TokenType::Equal => {
                    if self.right_side {
                        self.error("Two equal signs");
                        token_opt = self.tokens.read_next();
                        continue;
                    }
                    self.right_side = true;
                    token_opt = self.tokens.read_next();
                },
                TokenType::Plus | TokenType::Minus | TokenType::Num => {
                    let positive = token.ty != TokenType::Minus;

                    if token.ty != TokenType::Num {
                        token_opt = self.tokens.read_next();
                    }

                    let token = token_opt.as_ref().unwrap();

                    let parsed = token.value.parse();
                    if !parsed.is_ok() {
                        self.error(format!("Cannot parse floating point litteral {}", token.value));
                        token_opt = self.tokens.read_next();
                        continue;
                    }
                    let mut a: f64 = parsed.unwrap();
                    let mut p = 0;

                    if !positive { a *= -1.0 };

                    token_opt = self.tokens.read_next();
                    if let Some(TokenType::Mul) = token_opt.as_ref().map(|t| t.ty) {
                        token_opt = self.tokens.read_next();
                        if !self.expect_token(&token_opt, TokenType::Var) { token_opt = self.tokens.read_next(); continue; }
                        token_opt = self.tokens.read_next();

                        p = 1;

                        if let Some(TokenType::Exp) = token_opt.as_ref().map(|t| t.ty) {
                            token_opt = self.tokens.read_next();
                            if !self.expect_token(&token_opt, TokenType::Num) { token_opt = self.tokens.read_next(); continue; }
        
                            let token = token_opt.as_ref().unwrap();
                            let parsed = token.value.parse();
                            if !parsed.is_ok() {
                                self.error(format!("Cannot parse integer litteral {}", token.value));
                                continue;
                            }
                            p = parsed.unwrap();

                            token_opt = self.tokens.read_next();
                        }
                    }
    
                    if !self.right_side {
                        equation.left.push(Term { a, p });
                    } else {
                        equation.right.push(Term { a, p });
                    }
                },
                TokenType::Unknown => {
                    self.error(format!("Unknown token '{}'", token.value));
                    token_opt = self.tokens.read_next();
                },
                _ => {
                    self.error(format!("Unexpected token '{}'", token.value));
                    token_opt = self.tokens.read_next();
                },
            }

        }

        if !self.right_side {
            self.error("Missing equal sign");
        }

        if equation.left.len() == 0 {
            self.error("Missing left-hand side");
        }

        if equation.right.len() == 0 {
            self.error("Missing right-hand side");
        }

        if self.err { None }
        else { Some(equation) }
    }
}

fn reduce(equation: Equation) -> Equation {
    let mut res = Equation::default();

    let highest_exp = equation.left.iter()
        .chain(equation.right.iter())
        .map(|e| e.p)
        .max()
        .unwrap();

    for p in (0..=highest_exp).rev() {
        let left_sum = equation.left.iter()
            .filter(|e| e.p == p)
            .fold(0.0, |acc, v| acc + v.a);
        let right_sum = equation.right.iter()
            .filter(|e| e.p == p)
            .fold(0.0, |acc, v| acc + v.a);

        let a = left_sum - right_sum;
        if a != 0.0 {
            res.left.push(Term { a, p });
        }
    }

    if res.left.len() == 0 {
        res.left.push(Term { a: 0.0, p: 0 });
    }

    res.right.push(Term { a: 0.0, p: 0 });

    res
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
struct Complex(f64, f64);

impl Complex {
    fn real(r: f64) -> Complex {
        Complex(r, 0.0)
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0.0 && self.1 == 0.0 {
            write!(f, "0")?;
        } else if self.0 == 0.0 {
            write!(f, "{}i", self.1)?;
        } else if self.1 == 0.0 {
            write!(f, "{}", self.0)?;
        } else {
            write!(f, "{} ", self.0)?;
            if self.1 < 0.0 {
                write!(f, "- ")?;
            } else {
                write!(f, "+ ")?;
            }
            write!(f, "{}i", self.1.abs())?;
        }

        Ok(())
    }
}

impl From<(f64, f64)> for Complex {
    fn from((r, i): (f64, f64)) -> Complex {
        Complex(r, i)
    }
}

impl Sub for Complex {
    type Output = Complex;

    fn sub(self, other: Complex) -> Complex {
        Complex(self.0 - other.0, self.1 - other.1)
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, other: Complex) -> Complex {
        Complex(self.0 + other.0, self.1 + other.1)
    }
}

impl Mul for Complex {
    type Output = Complex;

    // (a+bi)(c+di) = ac + adi + bci - bd
    fn mul(self, other: Complex) -> Complex {
        Complex(self.0 * other.0 - self.1 * other.1, self.0 * other.1 + self.1 * other.0)
    }
}

impl Div for Complex {
    type Output = Complex;

    // (a,b)/(c,d) = ((ac+bd)/(c^2+d^2),(bc-ad)/(c^2+d^2))
    fn div(self, other: Complex) -> Complex {
        Complex(
            (self.0 * other.0 + self.1 * other.1) / (other.0.powi(2)+other.1.powi(2)),
            (self.1 * other.0 - self.0 - other.1) / (other.0.powi(2)+other.1.powi(2)),
        )
    }
}

impl Neg for Complex {
    type Output = Complex;

    fn neg(self) -> Complex {
        Complex(-self.0, -self.1)
    }
}

fn print_solutions(equation: Equation) {
    let highest_exp = equation.left.iter()
        .chain(equation.right.iter())
        .map(|e| e.p)
        .max()
        .unwrap();

    println!("Polynomial degree: {}", highest_exp);

    if highest_exp > 2 {
        println!("Cannot solve polynomial degree greater than 2");
        return;
    }

    let a = equation.left.iter().find(|e| e.p == 2).map(|e| e.a).unwrap_or(0.0);
    let b = equation.left.iter().find(|e| e.p == 1).map(|e| e.a).unwrap_or(0.0);
    let c = equation.left.iter().find(|e| e.p == 0).map(|e| e.a).unwrap_or(0.0);

    if a == 0.0 && b == 0.0 {
        // no X variable
        if c == 0.0 {
            println!("X ∈ Z");
        } else {
            println!("X ∈ ∅");
        }
        return;
    }

    if a == 0.0 {
        let x = -c / b;
        println!("X ∈ {{ {} }}", x);
        return;
    }

    let delta = b.powi(2) - 4.0 * a * c;

    if delta < 0.0 {
        println!("Δ < 0, there are therefore 2 complex solutions");

        let delta_sqrt: Complex = (0.0, (-delta).sqrt()).into();

        let az = Complex::real(a);
        let bz = Complex::real(b);
        let _cz = Complex::real(c);

        let x1 = (-bz - delta_sqrt) / (Complex::real(2.0) * az);
        let x2 = (-bz + delta_sqrt) / (Complex::real(2.0) * az);

        println!("X ∈ {{ {}, {} }}", x1, x2);
    } else if delta == 0.0 {
        println!("Δ = 0, there is therefore 1 real solution");

        let x0 = -b / (2.0 * a);

        println!("X ∈ {{ {} }}", x0);
    } else {
        println!("Δ > 0, there are therefore 2 real solutions");

        let x1 = (-b - delta.sqrt()) / (2.0 * a);
        let x2 = (-b + delta.sqrt()) / (2.0 * a);

        println!("X ∈ {{ {}, {} }}", x1, x2);
    }
}

fn main() {
    if let Some(arg) = env::args().nth(1) {
        let mut token_stream = TokenStream::new(arg.chars());
        let mut parser = Parser::new(&mut token_stream);
        if let Some(eq) = parser.parse() {
            println!("Equation: {}", eq);

            let reduced = reduce(eq);
            println!("Reduced: {}", reduced);

            print_solutions(reduced);
        }

    } else {
        eprintln!("Please provide an argument");
    }
}
