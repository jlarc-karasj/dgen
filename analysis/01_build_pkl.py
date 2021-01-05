"""
Author: Joshua Karas
Date: 11/17/2020
Project: Renewable Energy

Purpose: Export modified Washington Agents file in .pkl format
"""

import pandas as pd
import pickle as pkl

capex_merge = pd.read_csv("//securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen/analysis/capex_merge.csv")
 
import_file = open("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen/dgen_os/input_agents/agent_df_base_res_wa_revised.pkl", "rb")
df = pkl.load(import_file)
import_file.close()

df_merge = df.merge(capex_merge, how = "left", on = "county_id").set_index(df.index)

df_merge = df_merge.drop(columns = ["cap_cost_multiplier"])

df_merge = df_merge.rename(columns = {"capex_multiplier" : "cap_cost_multiplier"})

export_file = open("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen/dgen_os/input_agents/agent_df_base_res_wa_jlarc.pkl", "wb")
pkl.dump(df_merge, export_file)
export_file.close()