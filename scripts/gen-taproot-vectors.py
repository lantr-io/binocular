#!/usr/bin/env python3
"""gen-taproot-vectors.py — regenerate src/test/resources/fixtures/taproot-pegin-vectors.json.

Spec TRF-97 requires every Taproot derivation vector to be stored as data rather than as an
inline literal, because the same numbers must later prove that the Scala and the TypeScript
derivations agree. This script is the other half of that requirement: the numbers can be
REGENERATED rather than trusted.

Nothing here is imported from the Scala tree. This is pure-Python secp256k1, BIP-340 tagged
hashes, BIP-341 Taproot derivation and signature hashes, BIP-350 bech32m and Bitcoin
transaction serialisation, written from the BIPs. Agreement with `binocular` is therefore
evidence, not a tautology. The tree derivation follows `documentation/pegin_deposit.py` of
`ft-bifrost-bridge`, the reference for `documentation/bitcoin_tx_construction.md` §1.

Usage:
    python3 scripts/gen-taproot-vectors.py --check    # compare with the committed fixture
    python3 scripts/gen-taproot-vectors.py --write    # overwrite the committed fixture
    python3 scripts/gen-taproot-vectors.py            # print the JSON

Exit status is 1 when --check finds a difference.
"""
import argparse
import hashlib
import json
import os
import sys

# ---- secp256k1 ---------------------------------------------------------------
Pp = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
N = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
G = (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
     0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)


def inv(a, m):
    return pow(a, m - 2, m)


def point_add(p, q):
    if p is None:
        return q
    if q is None:
        return p
    if p[0] == q[0] and (p[1] != q[1] or p[1] == 0):
        return None
    if p == q:
        lam = (3 * p[0] * p[0] * inv(2 * p[1], Pp)) % Pp
    else:
        lam = ((q[1] - p[1]) * inv(q[0] - p[0], Pp)) % Pp
    x = (lam * lam - p[0] - q[0]) % Pp
    return (x, (lam * (p[0] - x) - p[1]) % Pp)


def point_mul(k, p=G):
    r = None
    while k:
        if k & 1:
            r = point_add(r, p)
        p = point_add(p, p)
        k >>= 1
    return r


def lift_x(x):
    """BIP-340 lift_x: the point with x coordinate `x` and an EVEN y."""
    y = pow((pow(x, 3, Pp) + 7) % Pp, (Pp + 1) // 4, Pp)
    return (x, y if y % 2 == 0 else Pp - y)


def x_only(point):
    return point[0].to_bytes(32, "big")


# ---- hashes ------------------------------------------------------------------
def sha256(b):
    return hashlib.sha256(b).digest()


def hash256(b):
    return sha256(sha256(b))


def tagged(tag, msg):
    t = sha256(tag.encode())
    return sha256(t + t + msg)


# ---- serialisation -----------------------------------------------------------
def varint(n):
    """Bitcoin CompactSize, in the shortest form consensus accepts."""
    if n < 0xFD:
        return bytes([n])
    if n <= 0xFFFF:
        return b"\xfd" + n.to_bytes(2, "little")
    if n <= 0xFFFFFFFF:
        return b"\xfe" + n.to_bytes(4, "little")
    return b"\xff" + n.to_bytes(8, "little")


def ser_outpoint(outpoint):
    """36 bytes: the txid in INTERNAL (little-endian) order, then the vout."""
    return outpoint["txid_le"] + outpoint["vout"].to_bytes(4, "little")


def ser_output(out):
    return out["value"].to_bytes(8, "little") + varint(len(out["spk"])) + out["spk"]


def serialize(tx, with_witness):
    """Consensus serialisation. A Taproot input's scriptSig is empty but still length-prefixed."""
    body = tx["version"].to_bytes(4, "little")
    if with_witness and any(i["witness"] for i in tx["inputs"]):
        body += b"\x00\x01"
    body += varint(len(tx["inputs"]))
    for i in tx["inputs"]:
        body += ser_outpoint(i) + varint(0) + i["sequence"].to_bytes(4, "little")
    body += varint(len(tx["outputs"]))
    for o in tx["outputs"]:
        body += ser_output(o)
    if with_witness and any(i["witness"] for i in tx["inputs"]):
        for i in tx["inputs"]:
            body += varint(len(i["witness"]))
            for item in i["witness"]:
                body += varint(len(item)) + item
    body += tx["lock_time"].to_bytes(4, "little")
    return body


# ---- BIP-341 signature hash --------------------------------------------------
SIGHASH_DEFAULT = 0x00


def sighash(tx, index, prevouts, leaf_hash=None):
    """The BIP-341 digest input `index` signs, under SIGHASH_DEFAULT and with no annex.

    `prevouts` is one `{"value", "spk"}` per input: the digest commits to the amount and the
    scriptPubKey of EVERY input, none of which appears in the transaction body.

    `leaf_hash` selects the spend path. `None` is a key-path spend; a TapLeaf hash is a
    script-path spend, whose extension commits to the leaf, the key version and the position
    of the last OP_CODESEPARATOR.
    """
    assert len(prevouts) == len(tx["inputs"]), "one prevout per input"

    sha_prevouts = sha256(b"".join(ser_outpoint(i) for i in tx["inputs"]))
    sha_amounts = sha256(b"".join(p["value"].to_bytes(8, "little") for p in prevouts))
    sha_spks = sha256(b"".join(varint(len(p["spk"])) + p["spk"] for p in prevouts))
    sha_sequences = sha256(
        b"".join(i["sequence"].to_bytes(4, "little") for i in tx["inputs"])
    )
    sha_outputs = sha256(b"".join(ser_output(o) for o in tx["outputs"]))

    # ext_flag is 1 for a script path and 0 for a key path; the annex is absent, so
    # spend_type = ext_flag * 2 + 0.
    spend_type = 2 if leaf_hash is not None else 0

    msg = bytes([SIGHASH_DEFAULT])
    msg += tx["version"].to_bytes(4, "little")
    msg += tx["lock_time"].to_bytes(4, "little")
    msg += sha_prevouts + sha_amounts + sha_spks + sha_sequences
    msg += sha_outputs
    msg += bytes([spend_type])
    msg += index.to_bytes(4, "little")
    if leaf_hash is not None:
        msg += leaf_hash + b"\x00" + (0xFFFFFFFF).to_bytes(4, "little")

    # The message is preceded by the epoch byte 0x00.
    return tagged("TapSighash", b"\x00" + msg)


# ---- bech32m (BIP-350) -------------------------------------------------------
_CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"


def _polymod(values):
    gen = [0x3B6A57B2, 0x26508E6D, 0x1EA119FA, 0x3D4233DD, 0x2A1462B3]
    chk = 1
    for v in values:
        top = chk >> 25
        chk = ((chk & 0x1FFFFFF) << 5) ^ v
        for i in range(5):
            chk ^= gen[i] if (top >> i) & 1 else 0
    return chk


def _convertbits(data, frm, to):
    acc = bits = 0
    out = []
    maxv = (1 << to) - 1
    for b in data:
        acc = (acc << frm) | b
        bits += frm
        while bits >= to:
            bits -= to
            out.append((acc >> bits) & maxv)
    if bits:
        out.append((acc << (to - bits)) & maxv)
    return out


def segwit_address(hrp, witver, program):
    const = 1 if witver == 0 else 0x2BC830A3
    data = [witver] + _convertbits(program, 8, 5)
    values = [ord(c) >> 5 for c in hrp] + [0] + [ord(c) & 31 for c in hrp] + data
    residue = _polymod(values + [0] * 6) ^ const
    checksum = [(residue >> 5 * (5 - i)) & 31 for i in range(6)]
    return hrp + "1" + "".join(_CHARSET[d] for d in data + checksum)


# ---- the peg-in Taproot tree -------------------------------------------------
def script_num_push(n):
    """Bitcoin Core's minimal CScript push of a non-negative integer.

    0 becomes OP_0. 1 to 16 become the single opcode bytes OP_1 to OP_16, `0x50 + n`. Only
    above 16 does a value get a length prefix and a little-endian body.

    The `_pushnum` of `documentation/pegin_deposit.py` is NOT minimal: it length-prefixes every
    value, so it writes 5 as `0105` instead of `55`. Do not "align" this function with it. A
    non-minimal push is a different leaf script, so a different leaf hash, output key and
    address, and nothing reports an error. `tuple_d_minimal_script_numbers` pins the correct
    encoding.
    """
    if n == 0:
        return b"\x00"
    if n <= 16:
        return bytes([0x50 + n])
    b = b""
    x = n
    while x:
        b += bytes([x & 0xFF])
        x >>= 8
    if b[-1] & 0x80:
        b += b"\x00"
    return bytes([len(b)]) + b


def csv_checksig_leaf(blocks, xonly):
    """`<blocks> OP_CSV OP_DROP <32-byte key> OP_CHECKSIG`, the one leaf shape this protocol uses."""
    return script_num_push(blocks) + bytes([0xB2, 0x75, 0x20]) + xonly + bytes([0xAC])


def tap_leaf_hash(script, leaf_version=0xC0):
    return tagged("TapLeaf", bytes([leaf_version]) + varint(len(script)) + script)


def pegin_tree(y51, y_federation, federation_csv, refund_timeout, q_auth, hrp):
    """Every derived value of one peg-in tree, as the fixture records them.

    The internal key is `Y_51`: the 51% quorum sweeps by key path and reveals no script. The
    two leaves are the federation's emergency sweep and the depositor's self-refund.
    """
    assert refund_timeout > federation_csv, "the federation window must open first"

    federation_leaf = csv_checksig_leaf(federation_csv, y_federation)
    refund_leaf = csv_checksig_leaf(refund_timeout, q_auth)
    federation_leaf_hash = tap_leaf_hash(federation_leaf)
    refund_leaf_hash = tap_leaf_hash(refund_leaf)

    # BIP-341 TapBranch hashes the two children in lexicographic order, so which leaf is
    # called "first" cannot affect the root.
    lo, hi = sorted([federation_leaf_hash, refund_leaf_hash])
    merkle_root = tagged("TapBranch", lo + hi)

    tweak = tagged("TapTweak", y51 + merkle_root)
    q = point_add(lift_x(int.from_bytes(y51, "big")), point_mul(int.from_bytes(tweak, "big")))
    output_key = x_only(q)
    parity = q[1] & 1
    script_pub_key = bytes([0x51, 0x20]) + output_key

    # A control block spending one leaf carries the OTHER leaf's hash as its merkle path,
    # because a two-leaf tree has exactly one sibling.
    prefix = bytes([0xC0 | parity]) + y51
    return {
        "y51": y51,
        "y_federation": y_federation,
        "q_auth": q_auth,
        "federation_csv_blocks": federation_csv,
        "pegin_refund_timeout_blocks": refund_timeout,
        "federation_leaf": federation_leaf,
        "refund_leaf": refund_leaf,
        "federation_leaf_hash": federation_leaf_hash,
        "refund_leaf_hash": refund_leaf_hash,
        "merkle_root": merkle_root,
        "tweak": tweak,
        "output_key": output_key,
        "output_parity": parity,
        "script_pub_key": script_pub_key,
        "address_human_readable_part": hrp,
        "address": segwit_address(hrp, 1, output_key),
        "refund_control_block": prefix + federation_leaf_hash,
        "federation_control_block": prefix + refund_leaf_hash,
    }


# ---- key material ------------------------------------------------------------
def key_from_byte(byte):
    """`x_only(d · G)` for `d = sha256(byte repeated 32 times)`, the fixture's construction."""
    d = sha256(bytes([byte]) * 32)
    return x_only(point_mul(int.from_bytes(d, "big")))


def bip86_output_key(xonly):
    """BIP-86: a wallet address commits no script tree, so the tweak message is the key alone."""
    t = tagged("TapTweak", xonly)
    q = point_add(lift_x(int.from_bytes(xonly, "big")), point_mul(int.from_bytes(t, "big")))
    return x_only(q)


Y51 = key_from_byte(0x01)
Y_FEDERATION = key_from_byte(0x03)
FEDERATION_CSV = 144
REFUND_TIMEOUT = 720
HRP = "tb"

# A CSV delay and a refund timeout that are both at or below 16, so BOTH leaf scripts push their
# block count as a single OP_N byte: OP_5 is 0x55 and OP_16 is 0x60. 144 and 720 are always
# length-prefixed, so no other tuple can catch a non-minimal push. 16 is the largest value with a
# single-byte opcode, so the pair also pins that boundary, and 16 > 5 satisfies
# PeginTreeParams.validate.
MINIMAL_FEDERATION_CSV = 5
MINIMAL_REFUND_TIMEOUT = 16

# (name, q_auth_source, q_auth, federation_csv_blocks, pegin_refund_timeout_blocks).
TUPLE_SOURCES = [
    (
        "tuple_a_parity_zero",
        "x_only(sha256(0xab repeated 32 times) * G)",
        key_from_byte(0xAB),
        FEDERATION_CSV,
        REFUND_TIMEOUT,
    ),
    (
        "tuple_b_parity_one",
        "x_only(sha256(0xac repeated 32 times) * G)",
        key_from_byte(0xAC),
        FEDERATION_CSV,
        REFUND_TIMEOUT,
    ),
    (
        "tuple_c_bip86_private_key_three",
        "BIP-86 taproot output key of private key d = 3, pinned in Bip322Spec",
        bip86_output_key(x_only(point_mul(3))),
        FEDERATION_CSV,
        REFUND_TIMEOUT,
    ),
    (
        "tuple_d_minimal_script_numbers",
        "tuple A's q_auth, reused so that ONLY the two timeouts differ from tuple A",
        key_from_byte(0xAB),
        MINIMAL_FEDERATION_CSV,
        MINIMAL_REFUND_TIMEOUT,
    ),
]


def p2tr(xonly):
    return bytes([0x51, 0x20]) + xonly


# ---- BIP-322 "simple" virtual transactions -----------------------------------
BIP322_MESSAGE = b"hello"


def bip322_vectors(output_key, message=BIP322_MESSAGE):
    """The BIP-322 `to_spend` txid and the key-path digest `to_sign` signs, for one output key.

    BIP-322 "simple" signs a fixed pair of VIRTUAL transactions that never reach a chain.
    `to_spend` is version 0 with one input over the null outpoint, whose scriptSig pushes
    `tagged_hash("BIP0322-signed-message", message)`, and one 0-value output paying the signer's
    P2TR. `to_sign` spends `to_spend:0` with sequence 0 and amount 0, and pays one 0-value
    OP_RETURN. The signature is then an ordinary BIP-341 key-path signature over `to_sign`, so
    `sighash` above computes it with no BIP-322 knowledge of its own.

    `to_spend` carries a scriptSig, which `serialize` never writes, so its bytes are laid out
    here by hand.

    Why this belongs in the fixture: a wallet's bip322-simple signature over these two digests is
    verified on-chain by `bifrost/bip322.ak`. Pinning them from THIS implementation gives the
    Scala BIP-341 code an anchor outside itself.
    """
    message_hash = tagged("BIP0322-signed-message", message)
    to_spend = (
        (0).to_bytes(4, "little")               # nVersion = 0
        + varint(1)                             # one input
        + b"\x00" * 32                          # prevout hash: the null outpoint
        + (0xFFFFFFFF).to_bytes(4, "little")    # prevout n
        + varint(34) + b"\x00\x20" + message_hash   # scriptSig = OP_0 PUSH32 <message hash>
        + (0).to_bytes(4, "little")             # nSequence = 0
        + varint(1)                             # one output
        + (0).to_bytes(8, "little")             # value 0
        + varint(34) + p2tr(output_key)         # scriptPubKey = OP_1 PUSH32 <Q>
        + (0).to_bytes(4, "little")             # nLockTime
    )
    to_spend_txid = hash256(to_spend)
    to_sign = {
        "version": 0,
        "lock_time": 0,
        "inputs": [{"txid_le": to_spend_txid, "vout": 0, "sequence": 0, "witness": []}],
        "outputs": [{"value": 0, "spk": bytes([0x6A])}],  # a bare OP_RETURN
    }
    prevouts = [{"value": 0, "spk": p2tr(output_key)}]
    return {
        "note": (
            "BIP-322 'simple' message signing over a Taproot key path, the shape a wallet's "
            "signMessage(msg, 'bip322-simple') produces and bifrost/bip322.ak verifies. "
            "to_spend is version 0, one input over the null outpoint with index 0xffffffff, "
            "scriptSig OP_0 PUSH32 <tagged_hash('BIP0322-signed-message', message)>, sequence "
            "0, one 0-value output paying OP_1 PUSH32 <output_key>, locktime 0. to_sign is "
            "version 0, locktime 0, one input spending to_spend:0 with sequence 0 and amount "
            "0, and one 0-value output with scriptPubKey 6a. sighash is the BIP-341 key-path "
            "digest of to_sign input 0, SIGHASH_DEFAULT, no annex. to_spend_txid is in DISPLAY "
            "(big-endian) order, as every other txid here is."
        ),
        "message": message.decode("ascii"),
        "message_hex": message.hex(),
        "message_hash": message_hash.hex(),
        "output_key": output_key.hex(),
        "output_key_source": (
            "tuple_a_parity_zero.q_auth. A hand-written test reaches for 0x11 repeated 32 "
            "times, which BIP-322 itself accepts, but that is not a valid x coordinate and "
            "bitcoin-s refuses to parse a witness v1 program over it."
        ),
        "to_spend_hex": to_spend.hex(),
        "to_spend_txid": to_spend_txid[::-1].hex(),
        "to_sign_unsigned_hex": serialize(to_sign, False).hex(),
        "sighash": sighash(to_sign, 0, prevouts).hex(),
    }


# ---- JSON emission -----------------------------------------------------------
def tuple_json(name, q_auth_source, tree):
    return {
        "name": name,
        "q_auth_source": q_auth_source,
        "y51": tree["y51"].hex(),
        "y_federation": tree["y_federation"].hex(),
        "q_auth": tree["q_auth"].hex(),
        "federation_csv_blocks": tree["federation_csv_blocks"],
        "pegin_refund_timeout_blocks": tree["pegin_refund_timeout_blocks"],
        "federation_leaf": tree["federation_leaf"].hex(),
        "refund_leaf": tree["refund_leaf"].hex(),
        "federation_leaf_hash": tree["federation_leaf_hash"].hex(),
        "refund_leaf_hash": tree["refund_leaf_hash"].hex(),
        "merkle_root": tree["merkle_root"].hex(),
        "tweak": tree["tweak"].hex(),
        "output_key": tree["output_key"].hex(),
        "output_parity": tree["output_parity"],
        "script_pub_key": tree["script_pub_key"].hex(),
        "address_human_readable_part": tree["address_human_readable_part"],
        "address": tree["address"],
        "refund_control_block": tree["refund_control_block"].hex(),
        "federation_control_block": tree["federation_control_block"].hex(),
    }


def build_fixture():
    trees = [
        (name, source, pegin_tree(Y51, Y_FEDERATION, csv, timeout, q, HRP))
        for name, source, q, csv, timeout in TUPLE_SOURCES
    ]
    tuple_a = next(t for name, _, t in trees if name == "tuple_a_parity_zero")

    return {
        "note": (
            "Peg-in Taproot tree test vectors. Internal key is y51. Leaf 1 is the federation "
            "sweep: <federation_csv_blocks> OP_CSV OP_DROP <y_federation> OP_CHECKSIG. Leaf 2 "
            "is the depositor refund: <pegin_refund_timeout_blocks> OP_CSV OP_DROP <q_auth> "
            "OP_CHECKSIG. The merkle root is TapBranch over the two leaf hashes in "
            "lexicographic order. refund_control_block spends the refund leaf and therefore "
            "carries federation_leaf_hash as its sibling; federation_control_block carries "
            "refund_leaf_hash. A block count is pushed with Bitcoin Core's MINIMAL encoding, "
            "so 1 to 16 are the single opcode bytes OP_1 to OP_16 and never a length-prefixed "
            "body; tuple_d_minimal_script_numbers is the tuple that exercises it."
        ),
        "key_derivation": (
            "y51 = x_only(sha256(0x01 repeated 32 times) * G); "
            "y_federation = x_only(sha256(0x03 repeated 32 times) * G)"
        ),
        "generated_by": (
            "scripts/gen-taproot-vectors.py. Run it with --check to confirm every value here "
            "still regenerates, or with --write to replace this file."
        ),
        "vectors": [tuple_json(name, source, tree) for name, source, tree in trees],
        "bip322": bip322_vectors(tuple_a["q_auth"]),
    }


FIXTURE = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "src", "test", "resources", "fixtures", "taproot-pegin-vectors.json",
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="compare with the committed file")
    parser.add_argument("--write", action="store_true", help="overwrite the committed file")
    args = parser.parse_args()

    fixture = build_fixture()
    path = FIXTURE
    text = json.dumps(fixture, indent=2) + "\n"

    if args.write:
        with open(path, "w") as f:
            f.write(text)
        print(f"wrote {path}")
        return 0

    if args.check:
        with open(path) as f:
            committed = json.load(f)
        if committed == fixture:
            print(f"OK: {path} regenerates exactly")
            return 0
        print(f"MISMATCH: {path} does not match the generator", file=sys.stderr)
        _report(committed, fixture)
        return 1

    print(text, end="")
    return 0


def _report(committed, generated, path=""):
    """Name every leaf that differs, so a mismatch says which value moved."""
    if isinstance(committed, dict) and isinstance(generated, dict):
        for key in sorted(set(committed) | set(generated)):
            _report(committed.get(key), generated.get(key), f"{path}.{key}")
    elif isinstance(committed, list) and isinstance(generated, list):
        if len(committed) != len(generated):
            print(f"{path}: {len(committed)} entries, generated {len(generated)}",
                  file=sys.stderr)
        for i, (c, g) in enumerate(zip(committed, generated)):
            _report(c, g, f"{path}[{i}]")
    elif committed != generated:
        print(f"{path}:\n  committed: {committed}\n  generated: {generated}", file=sys.stderr)


if __name__ == "__main__":
    sys.exit(main())
