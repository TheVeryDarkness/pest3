use pest3::typed::TypedParser;
use pest3_derive::Parser;

#[derive(Parser)]
#[grammar_inline = "
^ = \"\r\n\" | \"\n\"
field = (pest::ascii_digit | \".\" | \"-\")+
record = field - (\",\" - field)*
file = pest::SOI - record^* - pest::EOI
"]
pub struct CSVParser;

fn main() -> anyhow::Result<()> {
    let file: rules::file = CSVParser::try_parse("123.4,456\n789\n-1\n1,-2.3")?;
    let mut field_sum: f64 = 0.0;
    let mut record_count: u64 = 0;

    let records = file.record();

    for record in records {
        record_count += 1;
        let (head_field, tail_fields) = record.field();
        field_sum += head_field.span.as_str().parse::<f64>()?;
        for field in tail_fields {
            field_sum += field.span.as_str().parse::<f64>()?;
        }
    }

    println!("Sum of fields: {}", field_sum);
    println!("Number of records: {}", record_count);

    Ok(())
}
