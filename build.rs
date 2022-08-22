fn main() {
    let git_output = std::process::Command::new("git")
        .args(["rev-parse", "--git-dir"])
        .output()
        .ok();
    let git_dir = git_output.as_ref().and_then(|output| {
        std::str::from_utf8(&output.stdout)
            .ok()
            .and_then(|s| s.strip_suffix('\n').or_else(|| s.strip_suffix("\r\n")))
    });

    // Tell cargo to rebuild if the head or any relevant refs change.
    if let Some(git_dir) = git_dir {
        let git_path = std::path::Path::new(git_dir);
        let refs_path = git_path.join("refs");
        if git_path.join("HEAD").exists() {
            println!("cargo:rerun-if-changed={git_dir}/HEAD");
        }
        if git_path.join("packed-refs").exists() {
            println!("cargo:rerun-if-changed={git_dir}/packed-refs");
        }
        if refs_path.join("heads").exists() {
            println!("cargo:rerun-if-changed={git_dir}/refs/heads");
        }
        if refs_path.join("tags").exists() {
            println!("cargo:rerun-if-changed={git_dir}/refs/tags");
        }
    }

    let git_output = std::process::Command::new("git")
        .args([
            "describe",
            "--match=v*",
            "--abbrev=9",
            "--long",
            "--dirty=*",
        ])
        .output()
        .ok();
    let git_hash = git_output
        .as_ref()
        .and_then(|output| std::str::from_utf8(&output.stdout).ok().map(|s| s.trim()))
        // E.g. "v2.0.0-beta.2-7-g12ab34cd56*"
        //       ttttttttttttt N hhhhhhhhhhhD
        //  t => last matching annotated tag
        //  N => distance (in commits) from tag
        //  h => git commit id (hash)
        //  D => work tree dirty flag
        //
        // N.B. Since the tag may have '-' characters, the description string is parsed
        // from right to left.
        .and_then(|desc| {
            desc.rsplit_once('-').and_then(|(tag_and_distance, hash)| {
                let hash = hash
                    .strip_prefix('g')
                    .expect("git hash component starts with 'g'");
                let dirty = hash.ends_with('*');
                tag_and_distance
                    .rsplit_once('-')
                    .and_then(|(tag, distance_from_tag)| {
                        let tag = tag
                            .strip_prefix('v')
                            .expect("last annotated tag starts with 'v'");
                        let distance_from_tag = distance_from_tag
                            .parse::<usize>()
                            .expect("distance from tag is usize");
                        let cargo_pkg_version = env!("CARGO_PKG_VERSION");

                        if tag != cargo_pkg_version {
                            println!(
                                "cargo:warning=\
                                 version from Cargo.toml, \"{cargo_pkg_version}\", \
                                 does not match last annotated tag \"{tag}\""
                            );
                        }

                        if tag == cargo_pkg_version && distance_from_tag == 0 && !dirty {
                            // Do not use/display hash when the cargo version exactly
                            // matches the latest annotated tag.
                            None
                        } else {
                            Some(hash)
                        }
                    })
            })
        });

    // Make the current git hash available to the build.
    if let Some(git_hash) = git_hash {
        println!("cargo:rustc-env=STGIT_BUILD_GIT_HASH={git_hash}");
    }
}
