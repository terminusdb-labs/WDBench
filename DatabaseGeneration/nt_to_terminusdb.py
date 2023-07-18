import re
import sys
import csv
from unidecode import unidecode

if len(sys.argv) < 2:
    print("pass the file path")
    exit(1)

input_fname = sys.argv[1]
# csv_fname = sys.argv[2]
# graphql_exp = r'^[_a-zA-Z][_a-zA-Z0-9]*$'
# def graphql_label(label):
#     cleaned_label = unidecode(label)
#     cleaned_label = re.sub(r'[ -()/-–-.,+*]','_', cleaned_label)
#     assert re.match(graphql_exp,cleaned_label), f"Failed to create valid graphql name {label}"
#     return cleaned_label

# prop_dict = {}
# with open(csv_fname, 'r', encoding='utf-8') as csvfile:
#     reader = csv.DictReader(csvfile)
#     for row in reader:
#         row['graphql_label'] = graphql_label(row['label'])
#         prop_dict[row['property']] = row

# prefix
prefix_exp = re.compile('^@.*\.\s*')

# prop
p_exp = re.compile(r'^<http://www\.wikidata\.org/prop/direct/(P[0-9]+)>$')

# point
o_point_exp = re.compile(r'^".*Point\((-?[0-9\.Ee\+-]+) (-?[0-9\.Ee\+-]+)\)"\^\^<http://www\.opengis\.net/ont/geosparql#wktLiteral>$')

# math
o_math_exp = re.compile(r'^"((?:[^"\\]|\\.)*)"\^\^<http://www\.w3\.org/1998/Math/MathML>$')

# date
o_date_exp = re.compile(r'^"(-?[^-]+)-.*"\^\^<http://www.w3.org/2001/XMLSchema#dateTime>$')

oid = 0
with open(input_fname, 'r', encoding='utf-8') as input_file, \
     open('terminusdb.nt', 'w', encoding='utf-8') as output_file:
    for line in input_file:
        if prefix_exp.match(line):
            continue
        l = line.split(' ')
        s = l[0]
        p = l[1]
        o = ' '.join(l[2:-1])

        # m_p = p_exp.match(p)
        # if m_p is not None:
        #     prop_name = m_p.groups()[0]
        #     if prop_name in prop_dict:
        #         label = prop_dict[prop_name]['graphql_label']
        #         p = f'schema:{prop_name}'
        #     else:
        #         print(f"Fucked up property for triple\n{s} {p} {o}\n")
        #         continue

        m_o = o_point_exp.match(o)
        if m_o is not None:
            oid += 1
            lng = m_o.groups()[0]
            lat = m_o.groups()[1]
            output_file.write(f'{s} {p} <Point/{oid}> .\n')
            output_file.write(f'<Point/{oid}> rdf:type schema:Point .\n')
            output_file.write(f'<Point/{oid}> schema:type "Point_Type"^^xsd:string .\n')
            output_file.write(f'<Point/{oid}> schema:coordinates <Array/{oid}> .\n')
            output_file.write(f'<Array/{oid}> rdf:type sys:Array .\n')
            output_file.write(f'<Array/{oid}> sys:index "0"^^xsd:integer .\n')
            output_file.write(f'<Array/{oid}> sys:value "{lat}"^^xsd:decimal .\n')
            output_file.write(f'<Array/{oid}> sys:index "1"^^xsd:integer .\n')
            output_file.write(f'<Array/{oid}> sys:value "{lng}"^^xsd:decimal .\n')
            continue

        m_o = o_math_exp.match(o)
        if m_o is not None: 
            literal = m_o.groups()[0]
            output_file.write(f'{s} {p} "{literal}"^^xsd:string .\n')
            continue

        m_o = o_date_exp.match(o)
        if m_o is not None:
            year = m_o.groups()[0]
            # ridiculous to pretend we have dateTimes this old, it makes no sense.
            if abs(int(year)) > 260_000:
                output_file.write(f'{s} {p} "{year}"^^xsd:gYear .\n')
                continue

        output_file.write(line)
            
