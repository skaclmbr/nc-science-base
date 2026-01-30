############################################################################
## Author: Scott K Anderson
## Date : 2025-12-04
## Project : NC Science Base
## Purpose : convert JSON formatted records to excel spreadsheet

import os
import json
import copy
# from jsonschema import validate, ValidationError

from openpyxl import Workbook #allows connecting to databases
from openpyxl.worksheet.table import Table, TableStyleInfo
# from openpyxl.formatting.rule import FormulaRule
# from openpyxl.styles import Font, PatternFill
# from openpyxl.styles.differential import DifferentialStyle

nl = "\n"
ncsb_schema = {}
ncsb_schema_defs = {}
ncsb_data = {}

## create list of alpha cols for spreadsheet
alphabet_string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
alphabet_list = list(alphabet_string)
cols = alphabet_list
maxAlpha = "B"
for v in alphabet_list:
    for index in range(len(alphabet_string)):
        cols.append(v + cols[index])
    if v == maxAlpha:
        break

def get_ncsb_data():
    global ncsb_schema
    global ncsb_schema_defs
    global ncsb_data

    try: 
        # load ncsb file
        ncsb_data = json.load(
            open("ncsb.json", "r", encoding = "utf-8-sig")
        )

        # get schema from first item in list
        ncsb_schema = json.load(
            open("ncsb.schema.json", "r", encoding = "utf-8-sig")
        )

        return True
    except: 
        return False

# def validate_item(data):

#     try:
#         validate(instance = data, schema = ncsb_schema)
#         print("JSON data is valid according to the schema.")
#         return True

#     except ValidationError as e:
#         print(f"JSON validation error: {e.message}")
#         return False

#############################################################################
## EXCEL FUNCTIONS

# use schema to set up excel file
dirpath = os.path.dirname(os.path.abspath(__file__))
wb = Workbook()
ws = {} # dict to store worksheets
wsStructure = {"items": {}} # heading addresses for spreadsheets

ws["items"] = wb.active
ws["items"].title = "items"
wsColInd = {}
wsColInd["items"] = 0
wsRowInd = {}
wsRowInd["items"] = 2

style = TableStyleInfo(
    name="TableStyleMedium9",
    showFirstColumn=False,
    showLastColumn=False,
    showRowStripes=True,
    showColumnStripes=False
    )

def add_ws_col(sheetName, colName):
    if sheetName not in ws:
        ws[sheetName] = wb.create_sheet(sheetName)
        wsColInd[sheetName] = 0
        wsStructure[sheetName] = {}

        # add ID column
        colAddress = cols[wsColInd[sheetName]] + "1"
        ws[sheetName][colAddress] = "itemId"
        wsStructure[sheetName]["itemId"] =  cols[wsColInd[sheetName]]

        wsColInd[sheetName] += 1

    colAddress = cols[wsColInd[sheetName]] + "1"
    ws[sheetName][colAddress] = colName
    wsStructure[sheetName][colName] =  cols[wsColInd[sheetName]]

    wsColInd[sheetName] += 1

def add_ws_data(sheetName, colName, value):

    # sheet does not exist yet, create it
    if sheetName not in wsRowInd:
        wsRowInd[sheetName] = 2

    #add data to location if in the structure already
    if colName in wsStructure[sheetName]:
        colAddress = wsStructure[sheetName][colName] + str(wsRowInd[sheetName])
        ws[sheetName][colAddress] = value


def get_specs(ref):
    # get ref key
    key = ref.split("/")
    return ncsb_schema[key[1]][key[2]]

def walk_schema_recursive(data, path):
    # creates structure based on schema
    
    # get specification for reference
    if "$ref" in data:
        loopData = get_specs(data["$ref"])
    else:
        loopData = data

    # adjust indexes for finding 
    if len(path) > 2:
        sheetInd = 1
        colStartInd = 2
    else:
        sheetInd = 0
        colStartInd = 1

    # save col or recurse based on type
    match loopData["type"]:
        case "string" | "number" | "boolean":
            add_ws_col(path[sheetInd], ".".join(path[colStartInd:]))

        case "array":
            walk_schema_recursive(loopData["items"], path)

        case "object":
            for col, specs in loopData["properties"].items():
                walk_schema_recursive(specs, path + [col])


def walk_data_recursive(data, path = ["items"], itemId = ""):
    # adjust indexes for finding 
    if len(path) > 1:
        sheetInd = 1
    else:
        sheetInd = 0

    if isinstance(data, dict):
        for key, value in data.items():
            if isinstance(value, (dict, list)):
                # Recurse into nested dict/list
                walk_data_recursive(value, path + [key], itemId)
            else:
                add_ws_data(
                    path[sheetInd],
                    ".".join(path[sheetInd + 1:]+[key]),
                    value
                )
        
        # entire dict entered, add id, advance row
        sn = path[sheetInd]
        if sn != "items": # only add itemId if not "items" sheet
            colAddress = wsStructure[sn]["itemId"] + str(wsRowInd[sn])
            ws[sn][colAddress] = itemId
        
        #only advance row if 
        if len(path) <= 2: wsRowInd[sn] += 1

    elif isinstance(data, list):
        for item in data:
            if isinstance(item, (dict, list)):
                if itemId == "": itemId = item["id"]
                walk_data_recursive(item, path, itemId)  # Recurse into nested dict/list
            else:
                add_ws_data(path[sheetInd], key, value)

def main():
    success = get_ncsb_data()
    
    if success:
        # create xlsx structure and sheets
        schema = copy.deepcopy(ncsb_schema)
        schema.pop("required")
        schema.pop("$defs")
        walk_schema_recursive(schema, ["items"])        

        # populate data
        itemCount = 0
        for i in ncsb_data:
            walk_data_recursive(i, ["items"], i["id"])
            itemCount += 1

        print(
            itemCount,
            "items found"
        )
        ## save workbook
        wb.save(
            os.path.join(
                dirpath,
                "ncsb_data.xlsx"
            )
        )


if __name__ == "__main__":
    main()