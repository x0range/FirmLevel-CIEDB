import pandas as pd
import pickle
import feather
import pdb
import numpy as np

def get_province_code(row):
    ZIP = str(row['ZIP']).strip()
    #pdb.set_trace()
    try:
        zip_2d = ZIP[:2]
        code = zipdict[zip_2d]
    except:
        code = '86'        #'unknown'
    # exceptions from geographic pattern
    if code=='13' and ZIP[:4] in ('1374','1375','1376'): code='01'
    if code=='15' and ZIP[:4] in ('1626','1627','1628'): code='01'
    if code=='20' and ZIP[:4] in ('2024'): code='31'
    if code=='61' and ZIP[:4] in ('6173'): code='65'
    if code=='61' and ZIP[:4] in ('6247'): code='81'
    if code=='73' and ZIP[:4] in ('7354','7373'): code='01'
    if code=='75' and ZIP[:4] in ('7503'): code='01'
    return code

def get_province(row):
    #pdb.set_trace()
    code = row['Province Code']
    province = provincedict[code]
    return province

def clean_sectorcode(row):
    sectorcode = row['Sector']
    if not isinstance(sectorcode, str):
        if isinstance(sectorcode, float):
            sectorcode = int(sectorcode)
        sectorcode = str(sectorcode)
    if len(sectorcode) == 3 and sectorcode[0] in '6789':
        sectorcode = '0' + sectorcode
    try:
        assert len(sectorcode) == 4
        assert sectorcode.isnumeric()
    except:
        print("\n Incorrect sector code found: {}".format(sectorcode))
        #pdb.set_trace()
        sectorcode = ""
    return sectorcode

def get_2digitsector(row):
    sectorcode = row['Sector']
    if (len(sectorcode) == 4) and sectorcode.isnumeric():
        sectorcode_short = sectorcode[:2]
    else:
        sectorcode_short = ""
    return sectorcode_short

def get_age(row):
    #pdb.set_trace()
    cyear = row["Year"]
    #assert isinstance(cyear, int)
    fyear = row['Founding Year']
    if (isinstance(fyear, str)) and fyear.isnumeric():
        fyear = int(fyear)
    if isinstance(fyear, int):
        age = cyear - fyear
        if age < 0:
            age = None
    else:
        age = None
    return age

def clean_data(df):
    # cannot test for these in python cause python is horribly shit
    # drop negative or zero wages
    # drop negative or zero employment
    # drop negative or zero TOAS
    # drop FIAS>TOAS
    # drop negative ages
    # drop ID duplicates
    df_missing = df[(df["ID"] == "") | df["ID"].isna()] 
    df_present = df[~((df["ID"] == "") | df["ID"].isna())] 
    df = df_present.drop_duplicates()
    df = df.append(df_missing)
    
    # adjust column dtypes
    df.ZIP = df.ZIP.astype(str)
    #df.Sector = df.Sector.astype(str)
    df.ZIP[~df.ZIP.str.isnumeric()] = ""
    df.FirmType = df.FirmType.astype(str)
    df["Founding Year"] = df["Founding Year"].astype(str)
    df["Founding Month"] = df["Founding Month"].astype(str)
    df["TOAS"] = pd.to_numeric(df["TOAS"], errors='coerce')
    #df["FIAS"] = pd.to_numeric(df["FIAS"], errors='coerce')
    df["Employment"] = pd.to_numeric(df["Employment"], errors='coerce')
    return df

def strint(string):
    if isinstance(string, str) and string[-2:] == ".0":
        string = string[:-2]
    if isinstance(string, float):
        if np.isnan(string):
            string = ""
        else:
            string = int(string)
            string = str(string)
    return string

def all_columns_to_string(df):
    colnames = df.columns
    for column in colnames:
        df[column] = df[column]
    #(Pdb) feather.write_dataframe(df_merge, featherOutputFile)
    #(Pdb) fu = list(df_merge["EBTA Operating Profit"])
    #(Pdb) ft = [str(type(f)) for f in fu]
    #(Pdb) np.unique(ft, return_counts=True)
    return df
  

cols = [{"1998": 1, "1999": 1, "2000": 1, "2001": 1, "2002": 1, "2003": 1, "2004": 1, "2005": 1, "2006": 1, "2007": 1, "2008": None, "2009_35": 1, "2009_8": 1, "2010": 1, "2011": 1, "2012incl_EMPL": 1, "2013incl_EMPL": 1}, # ID
        {"1998": 13, "1999": 13, "2000": 13, "2001": 8, "2002": 10, "2003": 8, "2004": 13, "2005": 13, "2006": 13, "2007": 13, "2008": 11, "2009_35": 12, "2009_8": 7, "2010": 12, "2011": 12, "2012incl_EMPL": 7, "2013incl_EMPL": 18}, # Phone Number
        {"1998": 26, "1999": 25, "2000": 26, "2001": 19, "2002": 22, "2003": 17, "2004": 38, "2005": 38, "2006": 38, "2007": 38, "2008": 24, "2009_35": 25, "2009_8": 18, "2010": 25, "2011": 25, "2012incl_EMPL": 21, "2013incl_EMPL": 15}, # Founding Year
        {"1998": 27, "1999": 26, "2000": 27, "2001": 20, "2002": 23, "2003": 18, "2004": 39, "2005": 39, "2006": 39, "2007": 39, "2008": 25, "2009_35": 26, "2009_8": 19, "2010": 26, "2011": 26, "2012incl_EMPL": 22, "2013incl_EMPL": 16}, # Founding Month
        {"1998": 21, "1999": 21, "2000": 21, "2001": 17, "2002": 17, "2003": 13, "2004": 22, "2005": 22, "2006": 22, "2007": 22, "2008": 17, "2009_35": 18, "2009_8": 11, "2010": 18, "2011": 18, "2012incl_EMPL": 15, "2013incl_EMPL": 3}, # Sector
        {"1998": 17, "1999": 17, "2000": 17, "2001": 9, "2002": 13, "2003": 9, "2004": 16, "2005": 16, "2006": 16, "2007": 16, "2008": 14, "2009_35": 15, "2009_8": 8, "2010": 15, "2011": 15, "2012incl_EMPL": 11, "2013incl_EMPL": 22}, # ZIP code
        {"1998": 23, "1999": 22, "2000": 23, "2001": 18, "2002": 19, "2003": 14, "2004": 35, "2005": 35, "2006": 35, "2007": 35, "2008": 21, "2009_35": 22, "2009_8": 15, "2010": 22, "2011": 22, "2012incl_EMPL": 18, "2013incl_EMPL": 35}, # Reg Type
        {"1998": 73, "1999": 72, "2000": 73, "2001": 54, "2002": 67, "2003": 56, "2004": 88, "2005": 59, "2006": 59, "2007": 61, "2008": 62, "2009_35": 62, "2009_8": 55, "2010": 62, "2011": 62, "2012incl_EMPL": 77, "2013incl_EMPL": 90}, # Sales
        {"1998": 61, "1999": 60, "2000": 61, "2001": 42, "2002": 55, "2003": 44, "2004": 82, "2005": 56, "2006": 56, "2007": 79, "2008": 36, "2009_35": 36, "2009_8": 29, "2010": 36, "2011": 36, "2012incl_EMPL": 36, "2013incl_EMPL": 52}, # TOAS
        {"1998": 53, "1999": 52, "2000": 53, "2001": 34, "2002": 47, "2003": 37, "2004": 98, "2005": 70, "2006": 69, "2007": 72, "2008": 35, "2009_35": 35, "2009_8": 28, "2010": 35, "2011": 35, "2012incl_EMPL": 32, "2013incl_EMPL": 48}, # FIAS
        {"1998": 85, "1999": 84, "2000": 85, "2001": 66, "2002": 79, "2003": 67, "2004": 132, "2005": 103, "2006": 102, "2007": 105, "2008": 53, "2009_35": 53, "2009_8": 46, "2010": 53, "2011": 53, "2012incl_EMPL": 67, "2013incl_EMPL": 81}, # EBTA Operating Profit
        {"1998": 87, "1999": 86, "2000": 87, "2001": 68, "2002": 81, "2003": 69, "2004": 137, "2005": 107, "2006": 106, "2007": 109, "2008": 57, "2009_35": 57, "2009_8": 50, "2010": 57, "2011": 57, "2012incl_EMPL": 71, "2013incl_EMPL": 85}, # EBTA Gross Profit
        {"1998": 90, "1999": 89, "2000": 90, "2001": 69, "2002": 84, "2003": 72, "2004": 138, "2005": 110, "2006": 107, "2007": 112, "2008": 58, "2009_35": 58, "2009_8": 51, "2010": 58, "2011": 58, "2012incl_EMPL": 72, "2013incl_EMPL": 86}, # Tax
        {"1998": 57, "1999": 56, "2000": 57, "2001": 38, "2002": 51, "2003": 41, "2004": 102, "2005": 74, "2006": 73, "2007": 76, "2008": None, "2009_35": None, "2009_8": None, "2010": None, "2011": None, "2012incl_EMPL": 35, "2013incl_EMPL": 51}, # Deprecation
        {"1998": 84, "1999": 83, "2000": 84, "2001": 65, "2002": 78, "2003": 66, "2004": 131, "2005": 102, "2006": 101, "2007": 104, "2008": 52, "2009_35": 52, "2009_8": 45, "2010": 52, "2011": 52, "2012incl_EMPL": 63, "2013incl_EMPL": 77}, # Interest
        {"1998": 92, "1999": 91, "2000": 92, "2001": 71, "2002": 86, "2003": 73, "2004": 143, "2005": 116, "2006": 113, "2007": 118, "2008": None, "2009_35": None, "2009_8": None, "2010": None, "2011": None, "2012incl_EMPL": 73, "2013incl_EMPL": 87}, # Wages
        {"1998": 40, "1999": 44, "2000": 39, "2001": 27, "2002": 39, "2003": 29, "2004": 54, "2005": 52, "2006": 52, "2007": 52, "2008": 64, "2009_35": 64, "2009_8": 57, "2010": 64, "2011": 64, "2012incl_EMPL": 79, "2013incl_EMPL": 91}, # EMPL
        {"1998": 99, "1999": 98, "2000": 99, "2001": 78, "2002": 93, "2003": 80, "2004": 150, "2005": 123, "2006": 120, "2007": 125, "2008": None, "2009_35": None, "2009_8": None, "2010": None, "2011": None, "2012incl_EMPL": None, "2013incl_EMPL": None}, # Intermediate input
        {"1998": 42, "1999": 40, "2000": 41, "2001": 23, "2002": 35, "2003": 25, "2004": 87, "2005": 57, "2006": 57, "2007": 59, "2008": 63, "2009_35": None, "2009_8": 53, "2010": 60, "2011": 60, "2012incl_EMPL": 60, "2013incl_EMPL": 59}, # Output
        ]

colnamelist = ["ID", "Phone", "Founding Year", "Founding Month", "Sector", "ZIP", "FirmType", "Sales", "TOAS", "FIAS", "EBTA Operating Profit", "EBTA Gross Profit", "Tax", "Deprecation", "Interest", "Wages", "Employment", "Intermediate Input", "Output"]
final_columnlist = ["ID", "Phone_ZIP", "Phone", "ZIP", "Province Code", "Province", "Year", "Sector Short", "Sector", "FirmType", "Firm Age", "Founding Year", "Founding Month", "Sales", "TOAS", "FIAS", "EBTA Operating Profit", "EBTA Gross Profit", "Tax", "Deprecation", "Interest", "Wages", "Employment", "Intermediate Input", "Output"]
final_columnlist_merge = ["ID", "Phone_ZIP", "Phone", "ZIP", "Province Code", "Province", "Year", "Sector Short", "Sector", "FirmType", "Firm Age", "Founding Year", "Founding Month", "Sales", "TOAS", "FIAS", "EBTA Operating Profit", "EBTA Gross Profit", "Tax", "Deprecation", "Interest", "Wages", "Employment", "Intermediate Input", "Output", "ID_imputed"]
zipdict = {'01':'01','02':'01','03':'03','04':'03','05':'05','06':'05','07':'05','10':'10','11':'10','12':'10','13':'13','15':'15','16':'15','20':'20','21':'21','22':'21','23':'23','24':'23','25':'25','26':'25','27':'25','30':'30','31':'31','32':'31','33':'33','34':'33','35':'35','36':'35','40':'40','41':'41','42':'41','43':'43','44':'43','45':'45','46':'45','47':'45','51':'51','52':'51','53':'53','54':'53','55':'55','56':'55','57':'57','61':'61','62':'61','63':'61','64':'61','65':'65','66':'65','67':'65','71':'71','72':'71','73':'73','74':'73','75':'75','81':'81','83':'83','84':'83','85':'85','86':'86'}
provincedict = {'01':'Inner Mongolia','03':'Shanxi','05':'Hebei','10':'Beijing','13':'Jilin','15':'Heilongjiang','20':'Shanghai','21':'Jiangsu','23':'Anhui','25':'Shandong','30':'Tianjin','31':'Zhejiang','33':'Jiangxi','35':'Fujian','40':'Chongqing','41':'Hunan','43':'Hubei','45':'Henan','51':'Guangdong','53':'Guangxi','55':'Guizhou','57':'Hainan','61':'Sichuan','65':'Yunnan','71':'Shaanxi','73':'Gansu','75':'Ningxia','81':'Qinghai','83':'Xinjiang','85':'Tibet Autonomous Region','86':'unknown'}


#col_keys = [[] for i in range(len(cols))]
#col_keys_by_year = {year: [] for year in years}
#
#old_index_column = None
#df1 = pd.DataFrame()
#df2009 = pd.DataFrame()

def getframe(year):
    print("Parsing year {0}".format(year), end="\n")
    if year == 2009:
        yearAsStringList = ["2009_35", "2009_8"]
    elif year == 2012:
        yearAsStringList = ["2012incl_EMPL"]
    elif year == 2013:
        yearAsStringList = ["2013incl_EMPL"]        
    else:
        yearAsStringList = [str(year)]
    for yearAsString in yearAsStringList:
        filename = yearAsString + ".feather"
        df = feather.read_dataframe(filename)
        for i, col in enumerate(cols):
            colname_engl = colnamelist[i]
            if not col[yearAsString] is None:
                #column_name = df.columns[col[yearAsString]-1]
                #col_keys[i].append(column_name)
                #col_keys_by_year[year].append(column_name)
                df_columns = list(df.columns)
                df_columns[col[yearAsString]-1] = colname_engl
                df.columns = df_columns 
            else:
                df[colname_engl] = None
            if i==2:
                #pdb.set_trace()
                df[colname_engl] = df[colname_engl].apply(strint)
                #df[colname_engl] = df[colname_engl].replace("*","").astype(str).str.strip()
                #df.loc[(df[colname_engl]=="") | df[colname_engl].isna()] = "-1"
                #df[colname_engl] = df[colname_engl].astype(float).astype(int)
                #df[colname_engl] = df[colname_engl].astype(str)
                #df.loc[df[colname_engl]=="-1"] = None
                ##df[colname_engl] = df[colname_engl].astype(int)
            #print(df.head(2))
        
        # generate artificial columns
        # Second ID
        #column_name = df.columns[cols[0][yearAsString]-1]
        #column_name_ZIP = df.columns[cols[5][yearAsString]-1]
        column_name = "Phone"
        column_name_ZIP = "ZIP"
        #pdb.set_trace()
        if df[column_name].dtype == np.float64:
            df[column_name] = df[column_name].fillna(0)
            #df.loc[df[~column_name.isna()], column_name] = df.loc[df[~column_name.isna()], column_name].astype(int)
            #df.loc[df[~column_name.isna()], column_name] = df.loc[df[~column_name.isna()], column_name].astype(str)
            df[column_name] = df[column_name].astype(int)
            df[column_name] = df[column_name].astype(str)
        #try:
        #    dfd = df[~(df[column_name].isna() | (df[column_name]=="") | (df[column_name]==-1))]
        #except:
        #    pdb.set_trace()
        #dfd = dfd.copy()
        print("Parsing for year {0}: {1}".format(year, "Phone_ZIP"), end="\r")
        df["Phone_ZIP"] = df[column_name].str.strip() + "ZIP" + df[column_name_ZIP].map(str)
        # Observation year
        print("Parsing for year {0}: {1}".format(year, "Year     "), end="\r")
        df["Year"] = year
        # Apply Regions
        print("Parsing for year {0}: {1}".format(year, "Province "), end="\r")
        df["Province Code"] = df.apply(get_province_code, axis=1) 
        df["Province"] = df.apply(get_province, axis=1) 
        print("Parsing for year {0}: {1}".format(year, "Sector   "), end="\r")
        df["Sector"] = df.apply(clean_sectorcode, axis=1)
        df["Sector Short"] = df.apply(get_2digitsector, axis=1)
        print("Parsing for year {0}: {1}".format(year, "Firm Age "), end="\r")
        df["Firm Age"] = df.apply(get_age, axis=1)
        df = df[final_columnlist]
        if yearAsString == yearAsStringList[0]:
            df_return = df
        else:
            df_return.append(df)
    
    #pdb.set_trace()
    df_return = clean_data(df_return)
    df_merge = pd.merge(df_return, dflookup, left_on="Phone_ZIP", right_on="Phone_ZIP", how="left")
    df_merge.columns = final_columnlist_merge
    featherOutputFile = str(year) + "_reduced.feather"
    df_merge = df_merge.reset_index(drop=True)
    feather.write_dataframe(df_merge, featherOutputFile)
    print("Saving rows: {}".format(len(df_merge)))
    return df_return
        
phone_id_dict = {}
df_combined = pd.DataFrame(columns=final_columnlist)
years = list(range(1998, 2014))
#years = list(range(2012, 2014))

dflookup = pd.read_pickle("lookuptable.pkl")
dflookup = dflookup.drop_duplicates(["Phone_ZIP"])

for year in years:
    df = getframe(year)
    df_combined = df_combined.append(df)
    
df_merge = pd.merge(df_combined, dflookup, left_on="Phone_ZIP", right_on="Phone_ZIP", how="left")
df_merge.columns = final_columnlist_merge
featherOutputFile = "All_years_reduced_without_manual_matching.feather"
df_merge = df_merge.reset_index(drop=True)
#pdb.set_trace()
feather.write_dataframe(df_merge, featherOutputFile)


#raise SystemExit

dflookup = df_combined[["ID", "Phone_ZIP"]]
print(sum(dflookup.duplicated(["ID", "Phone_ZIP"])))
# 3425474
print(len(dflookup.duplicated(["ID", "Phone_ZIP"])))
# 4454511
dflookup2 = dflookup.drop_duplicates(["ID", "Phone_ZIP"], keep="first")
dflookup2 = dflookup.drop_duplicates(["Phone_ZIP"])
print(len(dflookup2))
# 1029037
# 3425474 + 1029037 == 4454511
print(sum(dflookup2.duplicated(["Phone_ZIP"])))
# 0
print(sum(dflookup2.duplicated(["ID"])))
# 170128
dflookup2.to_pickle("lookuptable.pkl")

#raise SystemExit

df_combined_merge = pd.merge(df_combined, dflookup2, left_on="Phone_ZIP", right_on="Phone_ZIP", how="left")
print(df_combined_merge.head())
#        ID_x           Phone_ZIP     Phone     ZIP Province Code Province  Year Sector FirmType Founding Year    ...       TOAS    FIAS EBTA Operating Profit  EBTA Gross Profit  Tax  Deprecation  Interest Wages  Employment       ID_y
#0  000000001  000000001ZIP547009  02718073  547009            53  Guangxi  1998   2672      120          1997    ...      12440  7440.0                  80.0               80.0  0.0          400     300.0     0         106  000000001
#1  000000002  000000002ZIP547008  02287405  547008            53  Guangxi  1998   3317      120          1985    ...       4050  3800.0                  15.0               15.0  0.0          300       0.0   580          80  000000002
#2  000000003  000000003ZIP547004  02922413  547004            53  Guangxi  1998   0912      120          1993    ...       3720  3360.0                 440.0              440.0  0.0            0       0.0   270          72  000000003
#3  000000004  000000004ZIP547004  02922413  547004            53  Guangxi  1998   0912      120          1993    ...       3060  2680.0                 570.0              570.0  0.0          310       0.0  1190         236  000000004
#4  000000005  000000005ZIP547004  00000000  547004            53  Guangxi  1998   0912      120          1993    ...       1640  1360.0                 280.0              280.0  0.0          150       0.0   170          45  000000005
#
#[5 rows x 22 columns]
df_combined_merge.columns = final_columnlist_merge
df_combined_merge = df_combined_merge.reset_index(drop=True)
featherOutputFile = "All_years_reduced.feather"
feather.write_dataframe(df_combined_merge, featherOutputFile)
