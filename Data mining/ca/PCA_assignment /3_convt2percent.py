import pandas as pd


# df = pd.read_csv('3a_removed_outlier_beds.csv')
# print df.shape

# categoric = ['Country','Hospital/Site']
# for col in df.columns:
# 	if col not in categoric:
# 		df[col] = pd.to_numeric(df[col],errors='coerce')
		
# 		print col
# 		print df[col].isnull().sum()

# print df.shape
# df.to_csv('3b_cast.csv')

df = pd.read_csv('3b_cast.csv')

for i,col in enumerate(df.columns[6:18]):
	print i,col
	df_col = df[col]
	df_col[df_col>1] /= 100
	df[col] = df_col

df.to_csv('3c_outlier_percents.csv')