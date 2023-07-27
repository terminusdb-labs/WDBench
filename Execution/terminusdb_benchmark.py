from requests.auth import HTTPBasicAuth
from python_graphql_client import GraphqlClient
import csv
import json
import time
from terminusdb_client import Client
from terminusdb_client import WOQLQuery as wq

GRAPHQL_ENDPOINT="http://localhost:6363/api/graphql/admin/wdbench"

GRAPHQL_FILES=["Queries/GraphQL/single_bgps.txt",
              "Queries/GraphQL/multiple_bgps.txt"]

# Instantiate the client with an endpoint.
auth = HTTPBasicAuth('admin', 'root')
client = GraphqlClient(endpoint=GRAPHQL_ENDPOINT,auth=auth)

for gf in GRAPHQL_FILES:
    with open(gf, 'r') as qf:
        print(f"Running queries: {gf}")
        with open(f"{gf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = row[1]
                query = "query{" + query +"}"
                print(f".{n}")
                # Synchronous request
                start_time = time.time()
                try:
                    data = client.execute(query=query)
                    elapsed_time = int((time.time() - start_time) * 1000)
                    if 'errors' in data:
                        print(data)
                    else:
                        results = len(data['data']['Node'])
                        csvwriter.writerow((n,elapsed_time,results))
                        print(f"..results: {results}")
                        tf.flush()
                except Exception as e:
                    print(e)

client = Client("http://localhost:6363/")
client.connect(db="wdbench")

WOQL_FILES=["Queries/WOQL/single_bgps.txt",
            "Queries/WOQL/multiple_bgps.txt",
            "Queries/WOQL/paths.txt"]
for wf in WOQL_FILES:
    with open(wf, 'r') as qf:
        with open(f"{wf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = json.loads(row[1])
                print(f".{n}")
                # Synchronous request
                start_time = time.time()
                try:
                    data = client.query(query)
                    elapsed_time = int((time.time() - start_time) * 1000)
                    results = len(data['bindings'])
                    print(f"..results: {results}")
                    csvwriter.writerow((n,elapsed_time,results))
                    tf.flush()
                except Exception as e:
                    print(e)
