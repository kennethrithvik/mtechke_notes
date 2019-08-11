import pandas as pd


df = pd.read_csv('1_removed_stars.csv')
print df.shape

# for i,col in enumerate(df.columns):
	# print i,col

categoric = ['Country','Hospital/Site']

for col in df.columns:
	if col not in categoric:
		for i,row in enumerate(df[col]):
			if type(row) == type('abc') and '%' in row:
				df.loc[i,col] = float(row[:-1])/100
		
		df[col] = pd.to_numeric(df[col],errors='coerce')
		print df[col].isnull().sum()
	
print df.shape
df.to_csv('2a_convert_to_numerical.csv')