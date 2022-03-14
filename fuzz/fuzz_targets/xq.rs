#![no_main]
use std::error::Error;

use libfuzzer_sys::fuzz_target;
use xq::{module_loader::PreludeLoader, run_query, util::SharedIterator, InputError, Value};

fuzz_target!(|qi: (&str, &str)| {
    let (query, input) = qi;
    || -> Result<(), Box<dyn Error>> {
        let input: SharedIterator<_> = serde_json::de::Deserializer::from_str(input)
            .into_iter::<Value>()
            .map(|r| r.map_err(InputError::new))
            .into_iter()
            .into();
        let _output = run_query(query, input.clone(), input, &PreludeLoader())?
            .collect::<Result<Vec<Value>, _>>()?;
        Ok(())
    }()
    .ok();
});
