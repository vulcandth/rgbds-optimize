#![forbid(unsafe_code)]

pub fn placeholder() -> &'static str {
    "optimize-core"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn placeholder_is_stable() {
        assert_eq!(placeholder(), "optimize-core");
    }
}
