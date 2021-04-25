# Extracted from the test framework for git, adapted for StGit.

# We always set GNUPGHOME, even if no usable GPG was found, as
#
# - It does not hurt, and
#
# - we cannot set global environment variables in lazy prereqs because they are
#   executed in an eval'ed subshell that changes the working directory to a
#   temporary one.

GNUPGHOME="$PWD/gpghome"
export GNUPGHOME

test_lazy_prereq GPG '
	gpg_version=$(gpg --version 2>&1)
	test $? != 127 || exit 1

	# As said here: http://www.gnupg.org/documentation/faqs.html#q6.19
	# the gpg version 1.0.6 did not parse trust packets correctly, so for
	# that version, creation of signed tags using the generated key fails.
	case "$gpg_version" in
	"gpg (GnuPG) 1.0.6"*)
		say "Your version of gpg (1.0.6) is too buggy for testing"
		exit 1
		;;
	*)
		# Available key info:
		# * Type DSA and Elgamal, size 2048 bits, no expiration date,
		#   name and email: C O Mitter <committer@example.com>
		# * Type RSA, size 2048 bits, no expiration date,
		#   name and email: Eris Discordia <discord@example.net>
		# No password given, to enable non-interactive operation.
		# To generate new key:
		#	gpg --homedir /tmp/gpghome --gen-key
		# To write armored exported key to keyring:
		#	gpg --homedir /tmp/gpghome --export-secret-keys \
		#		--armor 0xDEADBEEF >> lib-gpg/keyring.gpg
		#	gpg --homedir /tmp/gpghome --export \
		#		--armor 0xDEADBEEF >> lib-gpg/keyring.gpg
		# To export ownertrust:
		#	gpg --homedir /tmp/gpghome --export-ownertrust \
		#		> lib-gpg/ownertrust
		mkdir "$GNUPGHOME" &&
		chmod 0700 "$GNUPGHOME" &&
		(gpgconf --kill gpg-agent || : ) &&
		gpg --homedir "${GNUPGHOME}" --import \
			"$TEST_DIRECTORY"/lib-gpg/keyring.gpg &&
		gpg --homedir "${GNUPGHOME}" --import-ownertrust \
			"$TEST_DIRECTORY"/lib-gpg/ownertrust &&
		gpg --homedir "${GNUPGHOME}" </dev/null >/dev/null \
			--sign -u committer@example.com
		;;
	esac
'

sanitize_pgp() {
	perl -ne '
		/^-----END PGP/ and $in_pgp = 0;
		print unless $in_pgp;
		/^-----BEGIN PGP/ and $in_pgp = 1;
	'
}
