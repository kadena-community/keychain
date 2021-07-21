# keychain

## HD key signing tool for the Kadena blockchain

This tool is a low level command-line tool for signing Kadena transactions. It
is especially useful for cold wallet signing. It currently supports signing with
HD keys of the kind used by Kadena's Chainweaver wallet/IDE and is therefore a
backup alternative for situations where you have your recovery phrase but aren't
able to run the Chainweaver wallet itself. In the future this tool may support
more signing methods.

All you need to do is put your recovery phrase in a text file with spaces
separating each word and then run the following command:

```
echo <request-key> | ./keychain sign recovery-phrase.txt 0
```

The zero at the end is the index of the keys. 0 will give you the first key
generated from the recovery phrase, 1 will give you the next one, etc.

Here is the keychain help screen showing the available commands:

```
$ keychain --help
Usage: keychain COMMAND
  A key management system for the Kadena blockchain

Available options:
  -h,--help                Show this help text

Available commands:
  gen                      Generate a mnemonic phrase
  key                      Show a keypair from a mnemonic phrase
  sign                     Sign
  verify                   Verify
  list                     List keys
```

## Secure Recovery Phrase Generation

If you don't want to trust this program's key generation, you can use [the
included recovery phrase word list file](bip39-hex-dice.txt] to generate your
own recovery phrase from throwing 16-sided hexadecimal dice.  This notation

```
0/8 1c adjust
```

The first two numbers separated by the slash are the hex value of the first die.
The second two are the hex values of the rolls of the second and third dice
correspondinngly.

The above line means that if you roll either 01c or 81c, the word is "adjust".
Roll three hex dice twelve times to get a 12-word recovery phrase.
