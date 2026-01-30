############################################################################
## Author: Scott K Anderson
## Date : 2025-12-04
## Project : NC Science Base
## Purpose : convert Excel formatted records to JSON

import os
import json
import copy
# from jsonschema import validate, ValidationError

from openpyxl import Workbook, load_workbook #allows connecting to databases
from openpyxl.worksheet.table import Table, TableStyleInfo
# from openpyxl.formatting.rule import FormulaRule
# from openpyxl.styles import Font, PatternFill
# from openpyxl.styles.differential import DifferentialStyle

nl = "\n"
ncsb_schema_location = "https://ncpif.org/ncsb.schema.json"
ncsb_schema = {}
ncsb_schema_defs = {}
ncsb_dict = {}
rowInds = {}
wsStructure = {}
out = open("ncsb.json", "w", encoding = "utf-8-sig")

def load_schema():
    global ncsb_schema
    global wsStructure
    
    try: 
        # get schema from first item in list
        ncsb_schema = json.load(
            open("ncsb.schema.json", "r", encoding = "utf-8-sig")
        )

        return True
    except: 
        return False
      

def add_ws_data(
        sheet,
        itemId,
        colInd,
        value,
        rowInd,
        sheetType = "object"
    ):
    # pass column header and value, add to ncsb_dict
    global wsStructure
    global ncsb_dict
    global rowInds

    path = wsStructure[sheet][colInd].split(".")
    
    if path[-1] not in ["itemId"]: # don't include itemId
        # connect with appropriate sheet, set currDict
        currDict = ncsb_dict[itemId]
        if sheetType == "object":
            if sheet not in currDict:
                currDict[sheet] = {}
            currDict = currDict[sheet]
        elif sheetType == "array":
            if sheet not in currDict:
                currDict[sheet] = [] 

            if rowInd not in rowInds[sheet]:

                rowInds[sheet] = [rowInd]
                currDict[sheet].append({})

            currDict = currDict[sheet][-1]
        
        # loop through remaining path
        for key in path[:-1]:
            currDict = currDict.setdefault(key, {})

        # set value
        currDict[path[-1]] = value

    
#############################################################################
## EXCEL FUNCTIONS

# use schema to set up excel file
dirpath = os.path.dirname(os.path.abspath(__file__))
wb = Workbook()

cols = {} # dict of col letter and header name

def main():
    global ncsb_dict
    # get workbook
    wb = load_workbook(filename = "ncsb_data.xlsx")
    
    # load schema
    if load_schema():

        # loop through items sheet, get keys and data
        currSheet = "items"
        wsStructure[currSheet] = {}
        for rowInd, row in enumerate(wb[currSheet]):

            if rowInd == 0:
                for cell in row:
                    if len(cell.value) == 0: break 
                    # header row, collect headers and column values
                    wsStructure[currSheet][cell.col_idx] = cell.value
        
            else: #collect data
                itemId = row[0].value
                ncsb_dict[itemId] = {
                    "$schema" : ncsb_schema_location
                }

                for cell in row:
                    if cell.value:
                        ncsb_dict[itemId][wsStructure[currSheet][cell.col_idx]] = cell.value

        # loop through the rest of the sheets, add data to ncsb_dict
        sheets = wb.sheetnames
        sheets.pop(sheets.index("items"))
        for currSheet in sheets:
            # get the kind of data stored in sheet (object or list?)
            sheetSchema = ncsb_schema["properties"][currSheet]
            sheetType = ""
            if "type" in sheetSchema:
                if sheetSchema["type"] in ["object", "array"]:
                    sheetType = sheetSchema["type"]

            # loop through rows, save data to ncsb_dict
            for rowInd, row in enumerate(wb[currSheet]):
                if currSheet not in wsStructure: 
                    wsStructure[currSheet] = {}
                    rowInds[currSheet] = []

                if rowInd == 0:
                    for cell in row:
                        if not(cell.value): break
                        # header row, collect headers and column values
                        wsStructure[currSheet][cell.col_idx] = cell.value
            
                else: #collect data
                    itemId = row[0].value
                    for cell in row:
                        if cell.value:
                            add_ws_data(
                                currSheet,
                                itemId, # key for the originating item
                                cell.col_idx,
                                cell.value,
                                rowInd,
                                sheetType
                            )
                        else:
                            break





        # convert ncsb_data to list of dict objects
        ncsb_data = []
        for i in ncsb_dict: ncsb_data.append(ncsb_dict[i])

        # write to file
        out.write(json.dumps(ncsb_data, indent = 2))

if __name__ == "__main__":
    main()