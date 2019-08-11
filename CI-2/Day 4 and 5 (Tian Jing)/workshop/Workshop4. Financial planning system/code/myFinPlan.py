"""
Requires networkx (>= 1.9.0,<2.0.0), scipy (>= 0.9.0), numpy (>= 1.6.0), scikit-fuzzy 0.3
"""

# import numpy
import pandas as pd
from FinancialPlanner import FinancialPlanner
# import matplotlib.pyplot as plt

###############################################################################

def main():

    # Read customer data from csv if available
    customers = pd.read_csv("customers.csv")

    # instantiate financial planner
    myFinPlan = FinancialPlanner(customers)

    # evaluate each customer
    print('                                                        |-Investment Percentage-|')
    print("No RiskAbil RiskPref Liquidity Stocks  Property Savings  Stocks  Property Savings")
    for row,customer in zip(range(len(customers)),customers.iterrows()):
        consequents = myFinPlan.defuzzify(dict(customer[1]))
        # riskAbility, totalRiskPref, liquidityReq, stocks, properties, savings = myFinPlan.defuzzify(dict(customer[1]))
        print("%2i " % (row+1), end='')
        print("%.6f %.6f %.6f " % (consequents[0][0], consequents[0][1], consequents[0][2]), end='')
        print("%.6f %.6f %.6f " % (consequents[1][0], consequents[1][1], consequents[1][2]), end='')
        print("%.4f %.4f %.4f " % (consequents[2][0], consequents[2][1], consequents[2][2]))

    # display result
    # combineList = [ myFinPlan.defuzzify(dict(customer[1])) for row,customer in zip(range(len(customers)),customers.iterrows())]
    # pd.Series(combineList).head()

    return 0

if __name__ == "__main__":
    main()

###############################################################################

"""
Result:
"""