# -*- coding: utf-8 -*-
"""Read the cancer lexicon.

Reads ``cancer_lexicon.csv``, which the R pipeline reads too.

    from cancer_lexicon import KW, PAT, is_cancer
"""
import csv
import io
import os
import re

_HERE = os.path.dirname(os.path.abspath(__file__))
_CSV = os.path.join(_HERE, "cancer_lexicon.csv")


def load_lexicon(path=_CSV):
    """Return the lexicon patterns in source order."""
    with io.open(path, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.DictReader(fh))
    if not rows or "pattern" not in rows[0]:
        raise ValueError("cancer lexicon is missing a 'pattern' column: %s" % path)
    return [r["pattern"] for r in rows]


KW = load_lexicon()
PAT = [re.compile(k) for k in KW]


def is_cancer(fields):
    """True if any field matches any lexicon pattern.

    Fields shorter than 3 characters are ignored, as in
    R/00_build_classification.R.
    """
    for text in fields:
        if text and len(text) >= 3:
            for pattern in PAT:
                if pattern.search(text):
                    return True
    return False
