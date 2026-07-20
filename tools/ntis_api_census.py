# -*- coding: utf-8 -*-
"""Build the NTIS project census that this study analyses.

Queries every historical R&D ministry name plus broad backstop terms, pages
through all results, de-duplicates by project number, and writes the raw census
to data/raw/. This is the input to R/00_build_classification.R.

Requires the environment variable NTIS_API_KEY (register at ntis.go.kr for a
통합인증키). Keys are restricted to the registering institution's IP range.

    python tools/ntis_api_census.py

Runtime is several hours and the output is roughly 550 MB. NTIS registers
projects retroactively, so a census pulled today will contain more recent-year
projects than the 2026-07-13 pull the paper analyses. See "Limits on
reproduction" in README.md.
"""
import os, re, csv, sys, time, argparse, urllib.parse, urllib.request
import xml.etree.ElementTree as ET
from collections import defaultdict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "lexicon"))
from cancer_lexicon import is_cancer  # canonical lexicon, shared with the R pipeline

try:
    KEY = os.environ["NTIS_API_KEY"]
except KeyError:
    raise SystemExit("NTIS_API_KEY is not set. Never hardcode the key; export it instead:\n"
                     "  bash:        export NTIS_API_KEY=...\n"
                     "  PowerShell:  $env:NTIS_API_KEY = '...'")
BASE="https://www.ntis.go.kr/rndopen/openApi/public_project"
TAG=re.compile(r"<[^>]+>")
def clean(s): return "" if s is None else re.sub(r"\s+"," ",TAG.sub("",s)).strip()

# every historical R&D ministry (from the ministryname facet) + broad backstops so a
# project missed by its ministry token is still caught. dedupe by ProjectNumber.
MINISTRIES=["과학기술정보통신부","교육부","중소벤처기업부","중소기업청","교육과학기술부","농촌진흥청",
 "산업통상자원부","미래창조과학부","보건복지부","지식경제부","산업자원부","과학기술부","교육인적자원부",
 "농림축산식품부","환경부","해양수산부","다부처","산업통상부","식품의약품안전처","국토교통부","산림청",
 "국무조정실","농림수산식품부","식품의약품안전청","농림부","기상청","정보통신부","문화체육관광부","국토해양부",
 "원자력안전위원회","질병관리청","방위사업청","행정안전부","보건복지가족부","특허청","경찰청","소방청",
 "고용노동부","여성가족부","문화재청","방송통신위원회","통계청","국가보훈처","새만금개발청","인사혁신처"]
BACKSTOP=["및","연구","기술","개발"]
SEEDS=MINISTRIES+BACKSTOP

RAW=os.path.join(os.path.dirname(os.path.abspath(__file__)),"..","data","raw",
                 "ntis_api_census_260713.csv")
os.makedirs(os.path.dirname(RAW),exist_ok=True)
FIELDS=["ProjectNumber","Year","Ministry","ManageAgency","Title_KR","Title_EN","Keyword_KR",
        "Keyword_EN","GovFunds","TotalFunds","DevStage","SciLarge","SciMedium","SciSmall","PerformAgent"]

def get(url):
    for a in range(6):
        try:
            with urllib.request.urlopen(url,timeout=120) as r: return r.read().decode("utf-8")
        except Exception:
            if a==5: raise
            time.sleep(2*(a+1))
def page(q,start):
    url=BASE+"?"+urllib.parse.urlencode({"apprvKey":KEY,"collection":"project","query":q,
                                         "displayCnt":1000,"startPosition":start})
    root=ET.fromstring(get(url)); return int(root.findtext("TOTALHITS") or "0"), root.findall("./RESULTSET/HIT")

def run():
    seen=set()
    ntot=defaultdict(int); gov=defaultdict(float); tf=defaultdict(float)
    ncan=defaultdict(int); cgov=defaultdict(float)
    f=open(RAW,"w",newline="",encoding="utf-8-sig"); w=csv.writer(f); w.writerow(FIELDS)
    for q in SEEDS:
        total=None; start=1; gaps=0
        while True:
            if total is not None and start>total: break
            t,hs=page(q,start)
            if total is None: total=t
            if not hs:
                ok=False
                for _ in range(5):
                    time.sleep(1.5); _,hs=page(q,start)
                    if hs: ok=True; break
                if not ok:
                    if start+1000>total: break
                    gaps+=1; start+=1000; continue
            for h in hs:
                pn=clean(h.findtext("ProjectNumber"))
                if not pn or pn in seen: continue
                seen.add(pn)
                def tx(p):
                    e=h.find(p); return clean(e.text) if e is not None else ""
                sc=["","",""]
                for s in h.findall("ScienceClass"):
                    if s.get("type")=="new" and s.get("sequence")=="1":
                        sc=[tx("ScienceClass[@sequence='1']/Large") or clean(s.findtext("Large")),
                            clean(s.findtext("Medium")),clean(s.findtext("Small"))]; break
                y=clean(h.findtext("ProjectYear")); g=clean(h.findtext("GovernmentFunds"))
                tfu=clean(h.findtext("TotalFunds"))
                t1,t2=tx("ProjectTitle/Korean"),tx("ProjectTitle/English")
                k1,k2=tx("Keyword/Korean"),tx("Keyword/English")
                w.writerow([pn,y,tx("Ministry/Name"),tx("ManageAgency/Name"),t1,t2,k1,k2,g,tfu,
                            tx("DevelopmentPhases"),sc[0],sc[1],sc[2],tx("PerformAgent")])
                try: yi=int(y)
                except: continue
                if 2006<=yi<=2024:
                    ntot[yi]+=1
                    try: gov[yi]+=float(g or 0)
                    except: pass
                    try: tf[yi]+=float(tfu or 0)
                    except: pass
                    if is_cancer([t1,t2,k1,k2]):
                        ncan[yi]+=1
                        try: cgov[yi]+=float(g or 0)
                        except: pass
            start+=1000; time.sleep(0.1)
        print("  seed '%s' total=%s gaps=%d cumUnique=%d cumTot0624=%d"%(q,total,gaps,len(seen),sum(ntot.values())),flush=True)
    f.close()
    # Coverage summary. The cancer column is a check on the pull; the
    # analysed counts come from R/00_build_classification.R onward.
    print("\nYEAR | total_projects | cancer_projects | gov_funding_bn | total_funding_tril")
    for y in range(2006, 2025):
        print("%d | %14d | %15d | %14.1f | %18.2f"
              % (y, ntot.get(y, 0), ncan.get(y, 0), gov.get(y, 0)/1e9, tf.get(y, 0)/1e12))
    print("\nproject-year records pulled : %d" % sum(ntot.values()))
    print("distinct projects           : %d" % len(seen))
    print("census written to           : %s" % RAW)
    print("\nNext: Rscript R/run_all.R")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Pull the NTIS project census (2006-2024).")
    ap.add_argument("--out", default=RAW, help="output CSV path (default: data/raw/)")
    args = ap.parse_args()
    RAW = args.out
    os.makedirs(os.path.dirname(os.path.abspath(RAW)), exist_ok=True)
    run()
