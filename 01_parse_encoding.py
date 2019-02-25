#!/usr/bin/python
# -*- coding: utf-8 -*-

import pandas as pd
import feather
import glob
import sqlite3
import os

def get_dbtab(dbname):
    dbtab = None
    if dbname == '1998.sql':
        dbtab = 'qy98'
    elif dbname == '1999.sql':
        dbtab = 'qy99'
    elif dbname == '2000.sql':
        dbtab = '\"2000\"'
    elif dbname == '2001.sql':
        dbtab = '\"01\"'
    elif dbname == '2002.sql':
        dbtab = '\"2002\"'
    elif dbname == '2003.sql':
        dbtab = '\"03\"'
    elif dbname == '2004.sql':
        dbtab = '\"2004\"'
    elif dbname == '2005.sql':
        dbtab = '\"05年企业\"'
    elif dbname == '2006.sql':
        dbtab = '\"qy06(1)\"'
    elif dbname == '2007.sql':
        dbtab = 'Qy2007'
    elif dbname == '2008.sql':    
        dbtab = 'qy08'
    return dbtab

def read_dtas(fname):
    savename = fname.split(".")[0] + ".feather"

    with open(fname, "rb") as pa:
        df = pd.read_stata(pa)

    df2 = df.copy()
    cols = []
    for i in range(len(df.columns)):
        try:
            cols.append(df.columns[i].encode('latin-1').decode('gb18030'))
        except:
            cols.append("")

    if cols[22] == "":
        cols[22] = '登记注册机关级鸸ど绦姓管聿棵'

    df2.columns = cols

    print(df2.columns)
    ##E.g.:
    #Index(['_组织机构代码', '_单位详细名称', '_行业代码', '_主要业务活动1', '_主要业务活动2', '_主要业务活动3',
    #       '_行政区划代码', '_省', '_地', '_县', '_乡', '_地址', '_街道办事处', '_法定代表人', '_开业年',
    #       '_开业月', '_区号', '_固定电话', '_分机号', '_传真号码', '_传真分机号', '_邮政编码',
    #       '登记注册机关级鸸ど绦姓管聿棵', '_登记注册号工商行政管理部门', '_登记注册机关级别编制部门', '_登记注册号编制部门',
    #       '_登记注册机关级别民政部门', '_登记注册号民政部门', '_登记注册机关级别国家税务部门', '_登记注册号国家税务部门',
    #       '_登记注册机关级别地方税务部门', '_登记注册号地方税务部门', '_登记注册机关级别其他', '_登记注册号其他', '_登记注册类型',
    #       '_企业控股情况', '_隶属关系', '_企业营业状态', '_执行会计制度类别', '_代码', '_名称', '_年初存货',
    #       '_年初产成品', '_流动资产合计', '_应收账款', '_存货', '_产成品', '_固定资产合计', '_固定资产原价',
    #       '_累计折旧', '_本年折旧', '_资产总计', '_流动负债合计', '_应付账款', '_非流动负债合计', '_负债合计',
    #       '_所有者权益合计', '_实收资本', '_国家资本', '_集体资本', '_法人资本', '_个人资本', '_港澳台资本',
    #       '_外商资本', '_营业收入', '_主营业务收入', '_营业成本', '_主营业务成本', '_营业税金及附加',
    #       '_主营业务税金及附加', '_其他业务利润', '_销售费用', '_管理费用', '_税金', '_财务费用', '_利息收入',
    #       '_利息支出', '_资产减值损失', '_公允价值变动收益', '_投资收益', '_营业利润', '_营业外收入', '_补贴收入',
    #       '_营业外支出', '_利润总额', '_应交所得税', '_应付职工薪酬', '_应交增值税', '_工业总产值', '_工业销售产值',
    #       '_出口交货值'],
    #      dtype='object')

    for i, col in enumerate(cols):
        print(i, col)
        if df2[col].dtype == 'object':
            print("Object type found, attempting conversion")
            df2[col] = df2[col].apply(lambda x: x.encode('latin-1').decode('gb18030'))

    feather.write_dataframe(df2, savename)

def read_sqls(fname):
    fnamelastcomponent = fname.split("/")[-1]
    savename = fnamelastcomponent.split(".")[0] + ".feather"
    dbtab = get_dbtab(fnamelastcomponent)
    if dbtab is not None:
        print("Parsing " + fnamelastcomponent)
        sqlcommand = 'select * from ' + dbtab
        conn = sqlite3.connect(fname)
        df = pd.read_sql(sqlcommand, conn)
        feather.write_dataframe(df, savename)
    else:
        print("Invalid db encountered " + fnamelastcomponent)
    
if __name__ == "__main__":
    """read dtas first"""
    dtanames = glob.glob("*.dta")
    
    for fname in dtanames:
        read_dtas(fname)
    
    """then read sqls"""
    sqlnames = glob.glob(os.path.expanduser("~/eecon/rift/china/98-07/*.sql"))
    
    for fname in sqlnames:
        read_sqls(fname)
