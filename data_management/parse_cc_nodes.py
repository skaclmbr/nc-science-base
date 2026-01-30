# parse through nodes json and create csv files with data

import json
import re

nl = "\n"

test_fn = "data_management/test.json"
test = open(test_fn, "w", encoding = "utf-8-sig")

nodes_fn = "data_management/nodes.json"
# name, type, description, id

basic_fn = "data_management/cc_basic.csv"

# item_id, property, value
properties_fn = "data_management/cc_properties.csv"

# item_id, related_id, type
relationships_fn = "data_management/cc_relationships.csv"

basic_keys = [
    "name",
    "type",
    "description",
    "categories"
]
prefixes_to_remove = [
    "Has",
    "Is",
    "Was"
]

names = []
names_ids = []
basic = {}
# properties = {}

# {id: [{from: id, to: id, type: type, to_name: name}]}
# relationships = {}

def derive_id(t):
    #create id from name
    return t.lower().replace(" ", "-").replace(",","")

def remove_matching_prefix(text, prefixes):
    for prefix in prefixes:
        if text.startswith(prefix):
            return text[len(prefix):].strip()
    return text.strip()

def add_property(id, prop, val):

    prop = remove_matching_prefix(prop, prefixes_to_remove)

    basic[id]["properties"][prop] = val

    # if property is a priority, add index
    if prop == "NCWAP15 Index":
        basic[id]["basic"]["data1"] = val
    elif prop == "Priority Type":
        basic[id]["basic"]["data2"] = val
    elif prop == "Text":
        basic[id]["basic"]["data3"] = val


def add_relationship(id, to_id, to_name, type=""):

    if to_id not in basic[id]["relationships"]:
        basic[id]["relationships"][to_id] ={
                "from_id": id,
                "to_id": to_id,
                "type": type,
                "to_name": to_name
            }

def wikitext(id, t):
    # remove wikitext from string
    # find links
    links = re.findall(r'\[\[.*?\]\]', t)
    for link in links:
        # Process each link as needed
        # For example, you can add them to the relationships dictionary
        # Assuming 'id' and 'from_id' are available in the context
        text = re.sub(r'\[\[(.*)\]\]', r'\1', link)
        add_relationship(id, derive_id(text), text)

    type_match = re.search(r'\{\{.*?\}\}', t)
    if type_match:

        type = type_match.group(0)
        type = re.sub(r'\{\{(.*)\}\}', r'\1', type)
        type = re.sub(r'Page','',type).strip().lower()
        # Process the type as needed
        # add_property(id, "primary-type", type)
        if "type" not in basic[id]:
            basic[id]["type"] = type
    
    
    # Replace Markdown headers with HTML headers
    t = re.sub(r'======(.*)======', r'<h6>\1</h6>', t)
    t = re.sub(r'=====(.*)=====', r'<h5>\1</h5>', t)
    t = re.sub(r'====(.*)====', r'<h4>\1</h4>', t)
    t = re.sub(r'===(.*)===', r'<h3>\1</h3>', t)
    t = re.sub(r'==(.*)==', r'<h2>\1</h2>', t)
    t = re.sub(r'=(.*)=', r'<h1>\1</h1>', t)

    # Remove wikitext brackets
    result = re.sub(r'\[\[', '', t)
    result = re.sub(r'\]\]', '', result)
    result = re.sub(r'\{\{.*?\}\}', '', result)
    result = result.replace("\n", "")
    # result = re.sub(r'=.*?=', '', result)
    # result = re.sub(r'\{\{.*?\}\}', '', result)
    return result

def get_id (name):
    return names_ids[names.index(name)]

def get_name (id):
    return names[names_ids.index(id)]

def main():
    with open(nodes_fn, "r", encoding = "utf-8-sig") as f:
        cc_nodes = json.load(f)
        priority_id = 1
        for node in cc_nodes:
            #parse object
            # get/derive id
            if node["type"] == "Priority":
                # derive id differently
                node_id = f"ncwap15-{node['properties']['Has NCWAP15 Index']}-{derive_id(node['properties']['Has Priority Type'])}-{priority_id}"
                priority_id += 1
            else:
                node_id = derive_id(node["name"])

            if node_id not in basic:
                basic[node_id] = {
                    "basic" : {
                        "id" : node_id,
                        "data1" : "",
                        "data2" : "",
                        "data3" : ""
                    },
                    "properties" : {},
                    "relationships" : {},
                    "categories" : []
                }
            
            for k in basic_keys:
                # try:

                    if k == "name":
                        if node["type"] == "Priority":
                            basic[node_id]["basic"]["name"] = (
                                f"NCWAP15 {node['properties']['Has NCWAP15 Index']} {node['properties']['Has Priority Type']} {priority_id -1}")
                        
                        else:
                            names.append(node[k])
                            names_ids.append(node_id)
                            basic[node_id]["basic"][k] = node[k]
                    elif k == "description":
                        if node["type"] == "Priority":
                            basic[node_id]["basic"]["description"] = node["properties"]["Has Text"]
                        else:
                            basic[node_id]["basic"][k] = wikitext(node_id, node[k])
                    elif k == "type":
                        if "type" not in basic[node_id]:
                            basic[node_id]["basic"][k] = node[k].lower()
                    elif k == "categories":
                        basic[node_id]["categories"] = node[k]
                    else:
                        basic[node_id]["basic"][k] = node[k]
    
                # except:
                #     #key not found
                #     pass

            # properties

            for k, v in node["properties"].items():
                add_property(node_id, k,v)

            # relationships
            for v in node["edges"]:
                add_relationship(node_id, derive_id(v), v)


        test.write(json.dumps(basic))
        # print("basic", nl, json.dumps(basic))
    # export to csv files
    # basic
    with open(basic_fn, "w", encoding = "utf-8-sig") as f:
        f.write("item_id,name,type,description" + nl)
        for id in basic:
            f.write(
                ','.join(
                    [
                        id,
                        '"' + basic[id]['basic']['name'] + '",',
                        basic[id]['basic']['type'],
                        ',"' + basic[id]['basic']['description'] + '"'
                    ]
                ) + nl
            )

    # properties
    with open(properties_fn, "w", encoding = "utf-8-sig") as f:
        f.write("item_id,property,value" + nl)
        for id in basic:
            for p in basic[id]["properties"]:
                f.write(id + ',' + p + ',"' + str(basic[id]['properties'][p]) + '"' + nl)

    # categories
    with open("data_management/cc_categories.csv", "w", encoding = "utf-8-sig") as f:
        f.write("item_id,category" + nl)
        for id in basic:
            for c in basic[id]["categories"]:
                f.write(id + ',' + c + nl)


if __name__ == "__main__":
    main()