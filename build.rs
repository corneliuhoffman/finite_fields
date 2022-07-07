pub fn main() -> std::io::Result<()> {
  ocaml_build::Sigs::new("src/lib.ml").generate()
}