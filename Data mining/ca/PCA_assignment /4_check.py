import pandas as pd


df = pd.read_csv('4_imputed_mice.csv')
print df.shape

for i,col in enumerate(df.columns):
	print i,col
	print df[col].isnull().sum()
