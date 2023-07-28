from requests.auth import HTTPBasicAuth
from python_graphql_client import GraphqlClient
import csv
import json
import time
from terminusdb_client import Client
from terminusdb_client import WOQLQuery as wq
import signal
import re

LIMIT=100000 # Set to None if no limit is required

def graphql_limit(Query):
    if LIMIT:
        return re.sub('^Node\(',f"Node(limit:{LIMIT},", Query)

    return Query

def woql_limit(Query):
    if LIMIT:
        return { "@type" : "Limit",
                 "limit" : LIMIT,
                 "query" : Query }
    return Query

GRAPHQL_ENDPOINT="http://localhost:6363/api/graphql/admin/wdbench"
# Instantiate the client with an endpoint.
auth = HTTPBasicAuth('admin', 'root')
client = GraphqlClient(endpoint=GRAPHQL_ENDPOINT,auth=auth)

class Timeout(Exception):
    pass

def handler(signum, frame):
    print("TIMEOUT")
    raise Timeout

signal.signal(signal.SIGALRM, handler)

GRAPHQL_FILES=[] #["Queries/GraphQL/single_bgps.txt","Queries/GraphQL/multiple_bgps.txt"]
for gf in GRAPHQL_FILES:
    with open(gf, 'r') as qf:
        print(f"Running queries: {gf}")
        with open(f"{gf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = row[1]
                query = graphql_limit(query)
                query = "query{" + query +"}"
                print(f".{n}")
                # Synchronous request
                try:
                    signal.alarm(61)
                    start_time = time.time()
                    data = client.execute(query=query)
                    elapsed_time = int((time.time() - start_time) * 1000)
                    signal.alarm(0)
                    if 'errors' in data:
                        print(data)
                    else:
                        results = len(data['data']['Node'])
                        csvwriter.writerow((n,elapsed_time,results))
                        print(f"..results: {results}")
                        tf.flush()
                except Timeout:
                    csvwriter.writerow((n,-1,0))
                    print(f"..timeout")
                    tf.flush()
                except Exception as e:
                    signal.alarm(0)
                    print(e)

client = Client("http://localhost:6363/")
client.connect(db="wdbench")

WOQL_FILES=["Queries/WOQL/single_bgps.txt","Queries/WOQL/multiple_bgps.txt","Queries/WOQL/paths.txt"]
for wf in WOQL_FILES:
    with open(wf, 'r') as qf:
        print(f"Running queries: {wf}")
        with open(f"{wf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = json.loads(row[1])
                query = woql_limit(query)
                print(f".{n}")
                # Synchronous request
                try:
                    signal.alarm(61)
                    start_time = time.time()
                    data = client.query(query)
                    elapsed_time = int((time.time() - start_time) * 1000)
                    signal.alarm(0)
                    results = len(data['bindings'])
                    print(f"..results: {results}")
                    csvwriter.writerow((n,elapsed_time,results))
                    tf.flush()
                except Timeout:
                    csvwriter.writerow((n,-1,0))
                    print(f"..timeout")
                    tf.flush()
                except Exception as e:
                    signal.alarm(0)
                    print(e)
