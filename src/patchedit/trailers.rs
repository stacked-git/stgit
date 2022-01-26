use clap::ArgMatches;

use crate::{commit::CommitMessage, error::Error, stupid};

pub(crate) fn add_trailers<'a>(
    message: CommitMessage<'a>,
    matches: &ArgMatches,
    signature: &git2::Signature,
    autosign: Option<&str>,
) -> Result<CommitMessage<'a>, Error> {
    let mut trailers: Vec<(usize, &str, &str)> = vec![];

    for (opt_name, old_by_opt, trailer) in &[
        ("signoff", "sign-by", "Signed-off-by"),
        ("ack", "ack-by", "Acked-by"),
        ("review", "review-by", "Reviewed-by"),
    ] {
        let indices_iter = matches
            .indices_of(opt_name)
            .unwrap_or_default()
            .chain(matches.indices_of(old_by_opt).unwrap_or_default());

        let values_iter = matches
            .values_of(opt_name)
            .unwrap_or_default()
            .chain(matches.values_of(old_by_opt).unwrap_or_default());

        for (index, value) in indices_iter.zip(values_iter) {
            trailers.push((index, trailer, value))
        }
    }

    if trailers.is_empty() && autosign.is_none() {
        Ok(message)
    } else {
        let default_value = if let (Some(name), Some(email)) = (signature.name(), signature.email())
        {
            format!("{} <{}>", name, email)
        } else {
            return Err(Error::NonUtf8Signature(
                "trailer requires utf-8 signature".to_string(),
            ));
        };

        if let Some(autosign) = autosign {
            trailers.push((0, autosign, &default_value));
        }

        trailers.sort_by_key(|(index, _, _)| *index);

        let message_str = message.decode()?;
        let message_bytes = stupid::interpret_trailers(
            message_str.as_bytes(),
            trailers.iter().map(|(_index, trailer, value)| {
                if value.is_empty() {
                    (*trailer, default_value.as_str())
                } else {
                    (*trailer, *value)
                }
            }),
        )?;
        let message = String::from_utf8(message_bytes).map_err(|_| {
            Error::Generic("Could not decode message after adding trailers".to_string())
        })?;
        Ok(CommitMessage::from(message))
    }
}

#[cfg(test)]
mod test {
    use clap::{App, Arg};

    #[test]
    fn val_ind_occ() {
        let m = App::new("myapp")
            .arg(
                Arg::new("sign-off")
                    .long("sign-off")
                    .takes_value(true)
                    .min_values(0)
                    .default_missing_value("")
                    .require_equals(true)
                    .multiple_occurrences(true),
            )
            .arg(
                Arg::new("ack")
                    .long("ack")
                    .takes_value(true)
                    .min_values(0)
                    .default_missing_value("")
                    .require_equals(true)
                    .multiple_occurrences(true),
            )
            .arg(Arg::new("opt").short('o').multiple_occurrences(true))
            .get_matches_from(vec![
                "myapp",          // 0
                "-o",             // 1
                "--sign-off=AAA", // 2 3
                "--ack",          // 4 5 <- phantom index for default value
                "--sign-off",     // 6 7
                "-o",             // 8
                "--ack=BBB",      // 9 10
                "--sign-off=CCC", // 11 12
            ]);
        let sign_off_values = m.values_of("sign-off").unwrap();
        let sign_off_indices = m.indices_of("sign-off").unwrap();
        let sign_off_occurrences = m.occurrences_of("sign-off");
        let ack_values = m.values_of("ack").unwrap();
        let ack_indices = m.indices_of("ack").unwrap();
        let ack_occurrences = m.occurrences_of("ack");

        assert_eq!(3, sign_off_values.len());
        assert_eq!(3, sign_off_indices.len());
        assert_eq!(3, sign_off_occurrences);
        assert_eq!(vec![3, 7, 12], sign_off_indices.collect::<Vec<_>>());
        assert_eq!(vec!["AAA", "", "CCC"], sign_off_values.collect::<Vec<_>>());

        assert_eq!(2, ack_values.len());
        assert_eq!(2, ack_indices.len());
        assert_eq!(2, ack_occurrences);
        assert_eq!(vec![5, 10], ack_indices.collect::<Vec<_>>());
        assert_eq!(vec!["", "BBB"], ack_values.collect::<Vec<_>>());
    }
}
