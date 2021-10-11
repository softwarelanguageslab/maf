import requests
from requests.auth import HTTPBasicAuth
import os

"""
First install dependencies.

Usage: 
$ export GITHUB_USERNAME=something
$ export GITHUB_PERSONAL_ACCESS_TOKEN=something
$ python3 fetch_artifacts.py

Afterwards some zips will be in the "out" directory,
unzip them all such that they are in the directory
extracted/ZIP_NAME_WITHOUT_EXTENSION/ 

Now run graphs.py
"""

USERNAME = os.environ["GITHUB_USERNAME"]
TOKEN = os.environ["GITHUB_PERSONAL_ACCESS_TOKEN"]
def auth_req(url):
    r = requests.get(url, auth = HTTPBasicAuth(USERNAME,  TOKEN))
   return r.json()

def fetch_artifacts(): 
    page = 0
    output = []
    while True:
        data = auth_req(f"https://api.github.com/repos/softwarelanguageslab/maf/actions/artifacts?page={page}&per_page=100")    
        output.extend(data["artifacts"])
        if len(data["artifacts"]) == 0:
            break

        page = page + 1

    return output

all_artifacts = fetch_artifacts()
benchmarks = [ artifact for artifact in all_artifacts if artifact["name"] == "Performance evaluation results" ]


for benchmark in benchmarks:
     url = benchmark["archive_download_url"]
     name = benchmark["created_at"]+"_"+str(benchmark["id"])+".zip"
     os.system(f"curl -L -u {USERNAME}:{TOKEN} --output {name} {url}")

print("Done")

