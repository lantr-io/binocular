#!/usr/bin/env python3
"""Independent BIP32/86 public fixtures. No production wallet code or dependencies.

Uses the public BIP86 mnemonic and checks its published mainnet addresses:
https://github.com/bitcoin/bips/blob/master/bip-0086.mediawiki#test-vectors
Run with --check to compare the committed fixture, or no arguments to print it.
Never pass a real mnemonic to this test-only script.
"""
import hashlib
import hmac
import json
from pathlib import Path
import runpy
import sys

ec = runpy.run_path(str(Path(__file__).with_name("gen-taproot-vectors.py")))
MNEMONIC = "abandon " * 11 + "about"
HARDENED = 1 << 31


def vector(network, coin, hrp, index):
    seed = hashlib.pbkdf2_hmac("sha512", MNEMONIC.encode(), b"mnemonic", 2048)
    master = hmac.new(b"Bitcoin seed", seed, hashlib.sha512).digest()
    key, chain = int.from_bytes(master[:32], "big"), master[32:]
    for child in [86 | HARDENED, coin | HARDENED, HARDENED, 0, index]:
        point = ec["point_mul"](key)
        data = (b"\x00" + key.to_bytes(32, "big") if child & HARDENED else
                bytes([2 + point[1] % 2]) + ec["x_only"](point))
        derived = hmac.new(chain, data + child.to_bytes(4, "big"), hashlib.sha512).digest()
        tweak = int.from_bytes(derived[:32], "big")
        assert tweak < ec["N"]
        key, chain = (key + tweak) % ec["N"], derived[32:]
        assert key != 0
    internal = ec["x_only"](ec["point_mul"](key))
    output = ec["bip86_output_key"](internal)
    return dict(network=network, index=index, path=f"m/86'/{coin}'/0'/0/{index}",
                internal_key=internal.hex(), output_key=output.hex(),
                script_pub_key="5120" + output.hex(),
                address=ec["segwit_address"](hrp, 1, output))


vectors = [vector(network, coin, hrp, index)
           for network, coin, hrp in [("mainnet", 0, "bc"), ("testnet", 1, "tb"),
                                      ("regtest", 1, "bcrt")]
           for index in range(2)]
assert vectors[0]["address"] == "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr"
assert vectors[1]["address"] == "bc1p4qhjn9zdvkux4e44uhx8tc55attvtyu358kutcqkudyccelu0was9fqzwh"
fixture = dict(mnemonic=MNEMONIC, vectors=vectors)
if sys.argv[1:] == ["--check"]:
    path = Path(__file__).resolve().parent.parent / "src/test/resources/fixtures/bip86-wallet-vectors.json"
    assert json.loads(path.read_text()) == fixture, "BIP86 wallet fixture mismatch"
    print("BIP86 wallet fixtures match")
else:
    print(json.dumps(fixture, indent=2))
