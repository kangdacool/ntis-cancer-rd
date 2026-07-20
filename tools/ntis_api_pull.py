#!/usr/bin/env python
# -*- coding: utf-8 -*-
##################################################################
#####  NTIS national R&D project OpenAPI puller  #####
#  Automates the manual "SearchDownload" of NTIS project records so the
#  raw data step is reproducible (Reviewer B code/reproducibility ask) and
#  refreshable (e.g., a 2025 update after the Nov-N+1 full release).
#
#  Endpoint (confirmed 2026-07-13 from SNU IP):
#    https://www.ntis.go.kr/rndopen/openApi/public_project
#  Auth: apprvKey = NTIS 통합인증키  (READ FROM ENV, never hardcode/commit)
#    PowerShell:  $env:NTIS_API_KEY = '....'
#    The key is IP-restricted to the registered IP (our SNU 147.46.x) and
#    valid ~1 year; calls must originate from that machine.
#  Query: param is `query` (NOT searchWord); Korean must be UTF-8 (EUC-KR -> 0 hits).
#  Paging: displayCnt (page size, >=1000 honoured) + startPosition (deep paging OK).
#
#  Fields returned map 1:1 to the manual raw:
#    ProjectTitle/Korean -> Text1   ProjectTitle/English -> Text2
#    Keyword/Korean      -> Key1    Keyword/English      -> Key2
#    Ministry/Name       -> Inst_pay (다부처 = multiministerial)
#    ProjectYear         -> Year     GovernmentFunds     -> MoneyC (nominal KRW, project-level, NOT redacted)
#    DevelopmentPhases   -> research stage   ScienceClass(new,seq1) -> disease-site classification input
##################################################################
import os, sys, csv, time, argparse, re
import urllib.parse, urllib.request
import xml.etree.ElementTree as ET

BASE = "https://www.ntis.go.kr/rndopen/openApi/public_project"
TAG_RE = re.compile(r"<[^>]+>")           # strip <span class="search_word">..</span> highlights

def clean(s):
    if s is None:
        return ""
    return re.sub(r"\s+", " ", TAG_RE.sub("", s)).strip()

def api_key():
    k = os.environ.get("NTIS_API_KEY", "").strip()
    if not k:
        sys.exit("ERROR: set NTIS_API_KEY env var (NTIS 통합인증키). Never hardcode it.")
    return k

# ------------------------------------------------------------------ one page
def fetch_page(key, query, start, page):
    qs = urllib.parse.urlencode({
        "apprvKey": key, "collection": "project",
        "query": query, "displayCnt": page, "startPosition": start,
    })
    with urllib.request.urlopen(BASE + "?" + qs, timeout=90) as r:
        return r.read().decode("utf-8")

def _txt(hit, path):
    el = hit.find(path)
    return clean(el.text) if el is not None else ""

def parse_hits(xml):
    root = ET.fromstring(xml)
    total = int((root.findtext("TOTALHITS") or "0"))
    rows = []
    for hit in root.findall("./RESULTSET/HIT"):
        # ScienceClass type="new" sequence="1" (primary standard classification)
        sc_large = sc_med = sc_small = ""
        for sc in hit.findall("ScienceClass"):
            if sc.get("type") == "new" and sc.get("sequence") == "1":
                sc_large = _txt(sc, "Large"); sc_med = _txt(sc, "Medium"); sc_small = _txt(sc, "Small")
                break
        rows.append({
            "ProjectNumber": _txt(hit, "ProjectNumber"),
            "Text1":    _txt(hit, "ProjectTitle/Korean"),    # KR title
            "Text2":    _txt(hit, "ProjectTitle/English"),   # EN title
            "Key1":     _txt(hit, "Keyword/Korean"),         # KR keywords
            "Key2":     _txt(hit, "Keyword/English"),        # EN keywords
            "Inst_pay": _txt(hit, "Ministry/Name"),          # ministry (다부처 = multiministerial)
            "Year":     _txt(hit, "ProjectYear"),
            "MoneyC":   _txt(hit, "GovernmentFunds"),        # nominal KRW, project-level
            "TotalFunds": _txt(hit, "TotalFunds"),
            "Stage":    _txt(hit, "DevelopmentPhases"),      # 기초/응용/개발/기타
            "SciLarge": sc_large, "SciMedium": sc_med, "SciSmall": sc_small,
            "ManageAgency":   _txt(hit, "ManageAgency/Name"),
            "ResearchAgency": _txt(hit, "ResearchAgency/Name"),
            "PerformAgent":   _txt(hit, "PerformAgent"),
        })
    return total, rows

# ------------------------------------------------------------------ full pull
def pull(query, out, page=1000, limit=None, pause=0.3):
    key = api_key()
    start, got, total = 1, 0, None
    fields = ["ProjectNumber","Text1","Text2","Key1","Key2","Inst_pay","Year","MoneyC",
              "TotalFunds","Stage","SciLarge","SciMedium","SciSmall",
              "ManageAgency","ResearchAgency","PerformAgent"]
    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8-sig") as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader()
        while True:
            for attempt in range(3):
                try:
                    xml = fetch_page(key, query, start, page); break
                except Exception as e:
                    if attempt == 2:
                        raise
                    time.sleep(2 * (attempt + 1))
            total, rows = parse_hits(xml)
            if not rows:
                break
            for row in rows:
                w.writerow(row); got += 1
                if limit and got >= limit:
                    break
            print("  start=%d got=%d / total=%d" % (start, got, total), flush=True)
            if limit and got >= limit:
                break
            if start + page > total:
                break
            start += page
            time.sleep(pause)
    print("DONE: %d rows -> %s (TOTALHITS=%s)" % (got, out, total))
    return got

if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Pull NTIS project records via OpenAPI to CSV.")
    ap.add_argument("--query", required=True, help="search query (UTF-8 Korean OK)")
    ap.add_argument("--out", required=True, help="output CSV path")
    ap.add_argument("--page", type=int, default=1000, help="page size (default 1000)")
    ap.add_argument("--limit", type=int, default=None, help="stop after N rows (testing)")
    a = ap.parse_args()
    pull(a.query, a.out, page=a.page, limit=a.limit)
