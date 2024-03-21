use std::{
    env,
    iter,
    process,
    fs::File,
    path::{Path, PathBuf},
    fmt::{self, Display},
    collections::HashSet,
    num::{ParseIntError, ParseFloatError}
};

use crate::complain;


enum ArgError
{
    Parse(String),
    EnumParse{value: String, all: String},
    UnexpectedArg(String),
    DuplicateArg(String),
    MissingValue(String)
}

impl Display for ArgError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", match self
        {
            Self::Parse(x) => format!("error parsing {x}"),
            Self::EnumParse{value: x, all} => format!("error parsing {x}, available options: {all}"),
            Self::UnexpectedArg(x) => format!("unexpected argument {x}"),
            Self::DuplicateArg(x) => format!("duplicate argument {x}"),
            Self::MissingValue(x) => format!("missing value after {x} argument")
        })
    }
}

impl From<(&str, ParseIntError)> for ArgError
{
    fn from(value: (&str, ParseIntError)) -> Self
    {
        Self::Parse(value.0.to_owned())
    }
}

impl From<(&str, ParseFloatError)> for ArgError
{
    fn from(value: (&str, ParseFloatError)) -> Self
    {
        Self::Parse(value.0.to_owned())
    }
}

#[allow(dead_code)]
enum ArgType
{
    Variable,
    Flag(bool),
    Help
}

#[derive(Debug)]
enum ArgParseInfo
{
    Variable(String),
    Flag(bool)
}

impl ArgParseInfo
{
    pub fn flag(self) -> bool
    {
        match self
        {
            Self::Flag(x) => x,
            x => panic!("incorrect value: {x:?}")
        }
    }

    pub fn variable(self) -> String
    {
        match self
        {
            Self::Variable(x) => x,
            x => panic!("incorrect value: {x:?}")
        }
    }
}

struct ArgInfo<'a>
{
    value: Option<&'a mut dyn ArgParsable>,
    short: Option<char>,
    long: String,
    description: String,
    kind: ArgType,
    encountered: bool
}

impl<'a> ArgInfo<'a>
{
    pub fn help(&self, longest_arg: usize) -> String
    {
        let head = self.help_head();

        // this technically would overpad if the longest arg isnt a variable but wutever
        // i dont rly care
        let padded = longest_arg + "-a, --=VALUE".len();

        format!(" {head:<padded$}  {}", self.description)
    }

    fn help_head(&self) -> String
    {
        let mut line = match self.short
        {
            Some(c) => format!("-{c},"),
            None => "   ".to_owned()
        };

        line += &format!(" --{}", self.long);

        if let ArgType::Variable = self.kind
        {
            line += "=VALUE";
        }

        line
    }
}

struct ArgParser<'a>
{
    args: Vec<ArgInfo<'a>>
}

impl<'a> ArgParser<'a>
{
    pub fn new() -> Self
    {
        Self{args: Vec::new()}
    }

    pub fn push(
        &mut self,
        value: &'a mut dyn ArgParsable,
        short: impl Into<Option<char>>,
        long: impl Into<String>,
        description: impl Into<String>
    )
    {
        self.args.push(ArgInfo{
            description: description.into() + &Self::maybe_default(value),
            value: Some(value),
            short: short.into(),
            long: long.into(),
            kind: ArgType::Variable,
            encountered: false
        });
    }

    #[allow(dead_code)]
    pub fn push_flag(
        &mut self,
        value: &'a mut dyn ArgParsable,
        short: impl Into<Option<char>>,
        long: impl Into<String>,
        description: impl Into<String>,
        state: bool
    )
    {
        self.args.push(ArgInfo{
            description: description.into(),
            value: Some(value),
            short: short.into(),
            long: long.into(),
            kind: ArgType::Flag(state),
            encountered: false
        });
    }

    fn maybe_default(value: &mut dyn ArgParsable) -> String
    {
        match value.display_default()
        {
            Some(x) => format!(" (default {x})"),
            None => String::new()
        }
    }

    pub fn parse(mut self, mut args: impl Iterator<Item=String>) -> Result<(), ArgError>
    {
        self.args.push(ArgInfo{
            value: None,
            short: Some('h'),
            long: "help".to_owned(),
            description: "shows this message".to_owned(),
            kind: ArgType::Help,
            encountered: false
        });

        self.validate();

        while let Some(raw_arg) = args.next()
        {
            if let Some(arg) = raw_arg.strip_prefix("--")
            {
                if let Some(found) = self.args.iter_mut().find(|this_arg| this_arg.long == arg)
                {
                    if let ArgType::Help = found.kind
                    {
                        self.print_help();
                    }

                    Self::on_arg(&mut args, found, &raw_arg)?;
                } else
                {
                    return Err(ArgError::UnexpectedArg(raw_arg));
                }
            } else if let Some(arg) = raw_arg.strip_prefix('-')
            {
                if arg.len() != 1
                {
                    return Err(ArgError::UnexpectedArg(raw_arg));
                }

                let c = arg.chars().next().unwrap();

                if let Some(found) = self.args.iter_mut().find(|arg| arg.short == Some(c))
                {
                    if let ArgType::Help = found.kind
                    {
                        self.print_help();
                    }

                    Self::on_arg(&mut args, found, &raw_arg)?;
                } else
                {
                    return Err(ArgError::UnexpectedArg(raw_arg));
                }
            } else
            {
                return Err(ArgError::UnexpectedArg(raw_arg));
            }
        }

        Ok(())
    }

    fn print_help(self) -> !
    {
        println!("usage: {} [args]", env::args().next().unwrap());

        let longest_arg = self.args.iter().map(|arg| arg.long.len()).max()
            .unwrap_or(0);

        for arg in self.args
        {
            println!("{}", arg.help(longest_arg));
        }

        process::exit(0)
    }

    fn on_arg(
        mut args: impl Iterator<Item=String>,
        arg: &mut ArgInfo,
        arg_value: &str
    ) -> Result<(), ArgError>
    {
        if arg.encountered
        {
            return Err(ArgError::DuplicateArg(arg_value.to_owned()));
        }

        arg.encountered = true;

        let info = match arg.kind
        {
            ArgType::Variable =>
            {
                let value = args.next().ok_or_else(||
                {
                    ArgError::MissingValue(arg_value.to_owned())
                })?;

                ArgParseInfo::Variable(value)
            },
            ArgType::Flag(x) => ArgParseInfo::Flag(x),
            ArgType::Help => unreachable!()
        };

        arg.value.as_mut().unwrap().parse(info)?;

        Ok(())
    }

    fn validate(&self)
    {
        let short_args = self.args.iter()
            .filter_map(|arg| arg.short.as_ref());

        let c_set: HashSet<&char> = short_args.clone().collect();
        assert_eq!(c_set.len(), short_args.count());
    }
}

trait DisplayableDefault
{
    fn display_default(&self) -> Option<String>;
}

macro_rules! impl_displayable_default
{
    ($this_t:ident) =>
    {
        impl DisplayableDefault for $this_t
        {
            fn display_default(&self) -> Option<String>
            {
                Some(self.to_string())
            }
        }
    }
}

impl DisplayableDefault for PathBuf
{
    fn display_default(&self) -> Option<String>
    {
        Some(self.display().to_string())
    }
}

impl_displayable_default!{String}
impl_displayable_default!{bool}
impl_displayable_default!{f32}
impl_displayable_default!{f64}
impl_displayable_default!{usize}
impl_displayable_default!{u8}
impl_displayable_default!{u16}
impl_displayable_default!{u32}
impl_displayable_default!{u64}
impl_displayable_default!{u128}
impl_displayable_default!{isize}
impl_displayable_default!{i8}
impl_displayable_default!{i16}
impl_displayable_default!{i32}
impl_displayable_default!{i64}
impl_displayable_default!{i128}

impl<T: DisplayableDefault> DisplayableDefault for Option<T>
{
    fn display_default(&self) -> Option<String>
    {
        self.as_ref().and_then(|v| v.display_default())
    }
}

trait ParsableInner
where
    Self: Sized
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>;
}

trait ParsableEnum
{
    type Iter: Iterator<Item=Self>;


    fn iter() -> Self::Iter;
    fn as_string(&self) -> String;
    fn list_all() -> String;
}

macro_rules! iterable_enum
{
    (enum $enum_name:ident
    {
        $($key:ident),+
    }) =>
    {
        pub enum $enum_name
        {
            $($key,)+
        }

        impl $enum_name
        {
            const fn len() -> usize
            {
                [
                    $(stringify!($key),)+
                ].len()
            }
        }

        impl fmt::Display for $enum_name
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                write!(f, "{}", self.as_string())
            }
        }

        impl DisplayableDefault for $enum_name
        {
            fn display_default(&self) -> Option<String>
            {
                Some(self.as_string())
            }
        }

        impl ParsableEnum for $enum_name
        {
            type Iter = std::array::IntoIter<Self, { Self::len() }>;


            fn iter() -> Self::Iter
            {
                [
                    $(Self::$key,)+
                ].into_iter()
            }

            fn as_string(&self) -> String
            {
                match self
                {
                    $(Self::$key =>
                    {
                        let raw = stringify!($key);

                        let tail = raw.chars().skip(1).flat_map(|c|
                        {
                            if c.is_lowercase()
                            {
                                vec![c]
                            } else
                            {
                                iter::once('_').chain(c.to_lowercase()).collect::<Vec<_>>()
                            }
                        });

                        raw.chars().take(1).flat_map(char::to_lowercase).chain(tail)
                            .collect::<String>()
                    },)+
                }
            }

            fn list_all() -> String
            {
                Self::iter().map(|x| x.as_string()).reduce(|acc, x|
                {
                    acc + ", " + &x
                }).unwrap_or_default()
            }
        }
    }
}

iterable_enum!
{
    enum ProgramMode
    {
        Train,
        Run,
        Test,
        CreateDictionary,
        TrainEmbeddings,
        ClosestEmbeddings,
        WeightsImage
    }
}

impl<T: ParsableEnum> ParsableInner for T
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>
    {
        let value = value.to_lowercase();

        Self::iter().find(|x| x.as_string() == value)
            .ok_or_else(||
            {
                ArgError::EnumParse{value: value.to_owned(), all: Self::list_all()}
            })
    }
}

impl ParsableInner for PathBuf
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>
    {
        Ok(value.into())
    }
}

impl ParsableInner for String
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>
    {
        Ok(value.to_owned())
    }
}

impl ParsableInner for usize
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>
    {
        value.parse::<usize>().map_err(|err| (value, err).into())
    }
}

impl ParsableInner for f32
{
    fn parse_inner(value: &str) -> Result<Self, ArgError>
    {
        value.parse::<f32>().map_err(|err| (value, err).into())
    }
}

trait ArgParsable: DisplayableDefault
{
    fn parse(&mut self, value: ArgParseInfo) -> Result<(), ArgError>;
}

impl ArgParsable for bool
{
    fn parse(&mut self, value: ArgParseInfo) -> Result<(), ArgError>
    {
        *self = value.flag();

        Ok(())
    }
}

impl<T: ParsableInner + DisplayableDefault> ArgParsable for T
{
    fn parse(&mut self, value: ArgParseInfo) -> Result<(), ArgError>
    {
        *self = T::parse_inner(&value.variable())?;

        Ok(())
    }
}

impl<T: ParsableInner + DisplayableDefault> ArgParsable for Option<T>
{
    fn parse(&mut self, value: ArgParseInfo) -> Result<(), ArgError>
    {
        *self = Some(T::parse_inner(&value.variable())?);

        Ok(())
    }
}

pub struct Config
{
    pub input: Option<String>,
    pub output: String,
    pub width: usize,
    pub height: usize
}

impl Config
{
    pub fn parse(args: impl Iterator<Item=String>) -> Self
    {
        let mut input = None;
        let mut output = "tile.png".to_owned();

        let mut width: usize = 16;
        let mut height: usize = 16;

        let mut parser = ArgParser::new();

        parser.push(&mut input, 'i', "input", "input file");
        parser.push(&mut width, 'w', "width", "width of the image");
        parser.push(&mut height, 'H', "height", "height of the image");
        parser.push(&mut output, 'o', "output", "output file");

        parser.parse(args).unwrap_or_else(|err| complain(err));

        Self{
            input,
            width,
            height,
            output
        }
    }

    #[allow(dead_code)]
    fn get_file_inner(path: impl AsRef<Path>) -> File
    {
        let path = path.as_ref();

        File::open(path)
            .unwrap_or_else(|err|
            {
                complain(format!("give a valid file plz, cant open {} ({err})", path.display()))
            })
    }
}
