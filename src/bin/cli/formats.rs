#[derive(clap::ValueEnum, Clone, Debug)]
pub enum SerializationFormat {
    Json,
    Raw,
    Yaml,
}

#[derive(Clone, Debug)]
pub enum InputFormat {
    Simple(SerializationFormat),
    Slurp(SerializationFormat),
}

#[derive(Clone, Debug)]
pub enum OutputFormat {
    Simple(SerializationFormat),
    Compact(SerializationFormat),
    Pretty(SerializationFormat),
    Slurp(SerializationFormat),
}
