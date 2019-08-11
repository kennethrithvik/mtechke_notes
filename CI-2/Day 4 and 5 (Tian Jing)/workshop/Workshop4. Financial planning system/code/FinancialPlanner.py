"""
Requires networkx (>= 1.9.0,<2.0.0), scipy (>= 0.9.0), numpy (>= 1.6.0), scikit-fuzzy 0.3
"""

import numpy as np
import skfuzzy as fuzz
from skfuzzy import control as ctrl

###############################################################################
# constants

NUM_VARIABLES = 8  # index + 7 variables
DEBUGLEVEL = 0

###############################################################################

class FinancialPlanner:

    # constructor
    def __init__(self, data):

        if self.checkData(data) == False:
            return

        self.fuzzify(data)
        self.inference()

    # ------------------------------------------------------------------------------
    # This method verifies the correctness of the data:
    # (1) checks correct number of variables
    # (2) checks at least one row of data exists
    #
    def checkData(self, data):

        if len(data.columns) < NUM_VARIABLES:
            print("Insufficient fuzzy variables")
            return False

        if len(data) == 0:
            print("No data found")
            return False

        self.dataheaders = data.columns
        return True

    # ------------------------------------------------------------------------------
    # This method performs the fuzzification:
    # (1) sets the fuzzy partitions of each linguistic variable and
    # (2) sets the membership function of each linguistic term of the variable
    #
    def fuzzify(self, data):

        # set up monthly income
        x = np.arange(0, 10001, 100)
        self.monthlyIncome = ctrl.Antecedent(x, data.columns[1])
        self.monthlyIncome['lo'] = fuzz.trapmf(self.monthlyIncome.universe, [0, 0, 2000, 3000])
        self.monthlyIncome['me'] = fuzz.trapmf(self.monthlyIncome.universe, [2000, 3000, 7000, 8000])
        self.monthlyIncome['hi'] = fuzz.trapmf(self.monthlyIncome.universe, [7000, 8000, 10000, 10000])

        # set up total assets
        x = np.arange(0, 2000001, 1000)
        self.totalAssets = ctrl.Antecedent(x, data.columns[2])
        self.totalAssets['lo'] = fuzz.trapmf(self.totalAssets.universe, [0, 0, 200000, 300000])
        self.totalAssets['me'] = fuzz.trapmf(self.totalAssets.universe, [300000, 500000, 800000, 1000000])
        self.totalAssets['hi'] = fuzz.trapmf(self.totalAssets.universe, [800000, 1000000, 2000000, 2000000])

        # set up age
        x = np.arange(20, 101, 1)
        self.age = ctrl.Antecedent(x, data.columns[3])
        self.age['lo'] = fuzz.trapmf(self.age.universe, [20, 20, 30, 40])
        self.age['me'] = fuzz.trapmf(self.age.universe, [30, 40, 50, 60])
        self.age['hi'] = fuzz.trapmf(self.age.universe, [50, 60, 100, 100])

        # set up risk ability as a consequent
        x = np.arange(0, 11, 1)
        self.riskAbilityCon = ctrl.Consequent(x, 'riskAbilityCon')
        self.riskAbilityCon['lo'] = fuzz.trapmf(self.riskAbilityCon.universe, [0, 0, 3, 4])
        self.riskAbilityCon['me'] = fuzz.trapmf(self.riskAbilityCon.universe, [3, 4, 6, 7])
        self.riskAbilityCon['hi'] = fuzz.trapmf(self.riskAbilityCon.universe, [6, 7, 10, 10])

        if DEBUGLEVEL == 1:
            self.monthlyIncome.view()
            self.totalAssets.view()
            self.age.view()
            self.riskAbilityCon.view()

        #------------------------------------------------------------
        # set up risk ability as an antecedent
        x = np.arange(0, 11, 1)
        self.riskAbilityAnt = ctrl.Antecedent(x, 'riskAbilityAnt')
        self.riskAbilityAnt['lo'] = fuzz.trapmf(self.riskAbilityAnt.universe, [0, 0, 3, 4])
        self.riskAbilityAnt['me'] = fuzz.trapmf(self.riskAbilityAnt.universe, [3, 4, 6, 7])
        self.riskAbilityAnt['hi'] = fuzz.trapmf(self.riskAbilityAnt.universe, [6, 7, 10, 10])

        # set willingness to take risks
        # 0.0 very unwilling -> 1.0 very willing
        x = np.arange(0.0, 1.1, 0.1)
        self.takeRisk = ctrl.Antecedent(x, data.columns[4])
        self.takeRisk['lo'] = fuzz.trapmf(self.takeRisk.universe, [0, 0, 0.3, 0.4])
        self.takeRisk['me'] = fuzz.trapmf(self.takeRisk.universe, [0.3, 0.4, 0.6, 0.7])
        self.takeRisk['hi'] = fuzz.trapmf(self.takeRisk.universe, [0.6, 0.7, 1.0, 1.0])

        # set up total risk preference as a consequent
        x = np.arange(0, 11, 1)
        self.totalRiskPrefCon = ctrl.Consequent(x, 'totalRiskPrefCon')
        self.totalRiskPrefCon['lo'] = fuzz.trapmf(self.totalRiskPrefCon.universe, [0, 0, 3, 4])
        self.totalRiskPrefCon['me'] = fuzz.trapmf(self.totalRiskPrefCon.universe, [3, 4, 6, 7])
        self.totalRiskPrefCon['hi'] = fuzz.trapmf(self.totalRiskPrefCon.universe, [6, 7, 10, 10])

        if DEBUGLEVEL == 1:
            self.riskAbilityAnt.view()
            self.takeRisk.view()
            self.totalRiskPrefCon.view()

        #------------------------------------------------------------
        # set dependents' expenses MF
        # expected to incur dependents' expenses x years from now
        x = np.arange(0, 11, 1)
        self.dependentExp = ctrl.Antecedent(x, data.columns[5])
        self.dependentExp['lo'] = fuzz.trapmf(self.dependentExp.universe, [0, 0, 2, 3])
        self.dependentExp['me'] = fuzz.trapmf(self.dependentExp.universe, [2, 3, 5, 6])
        self.dependentExp['hi'] = fuzz.trapmf(self.dependentExp.universe, [5, 6, 10, 10])

        # set ratio of living expenses to total assets MF
        # in percentage point
        x = np.arange(0, 21, 1)
        self.ratio_LE_TA = ctrl.Antecedent(x, data.columns[6])
        self.ratio_LE_TA['lo'] = fuzz.trapmf(self.ratio_LE_TA.universe, [0, 0, 3, 5])
        self.ratio_LE_TA['me'] = fuzz.trapmf(self.ratio_LE_TA.universe, [3, 5, 10, 12])
        self.ratio_LE_TA['hi'] = fuzz.trapmf(self.ratio_LE_TA.universe, [10, 12, 20, 20])

        # set big item expenses MF
        # expected to spend on big items x years from now
        x = np.arange(0, 11, 1)
        self.bigItemExp = ctrl.Antecedent(x, data.columns[7])
        self.bigItemExp['lo'] = fuzz.trapmf(self.bigItemExp.universe, [0, 0, 2, 3])
        self.bigItemExp['me'] = fuzz.trapmf(self.bigItemExp.universe, [2, 3, 5, 6])
        self.bigItemExp['hi'] = fuzz.trapmf(self.bigItemExp.universe, [5, 6, 10, 10])

        # set up liquidity requirement as a consequent
        x = np.arange(0, 11, 1)
        self.liquidityReqCon = ctrl.Consequent(x, 'liquidityReqCon')
        self.liquidityReqCon['lo'] = fuzz.trapmf(self.liquidityReqCon.universe, [0, 0, 3, 4])
        self.liquidityReqCon['me'] = fuzz.trapmf(self.liquidityReqCon.universe, [3, 4, 6, 7])
        self.liquidityReqCon['hi'] = fuzz.trapmf(self.liquidityReqCon.universe, [6, 7, 10, 10])

        if DEBUGLEVEL == 1:
            self.dependentExp.view()
            self.bigItemExp.view()
            self.ratio_LE_TA.view()
            self.liquidityReqCon.view()

        #------------------------------------------------------------
        # set up liquidity requirement as an antecedent
        x = np.arange(0, 11, 1)
        self.liquidityReqAnt = ctrl.Antecedent(x, 'liquidityReqAnt')
        self.liquidityReqAnt['lo'] = fuzz.trapmf(self.liquidityReqAnt.universe, [0, 0, 3, 4])
        self.liquidityReqAnt['me'] = fuzz.trapmf(self.liquidityReqAnt.universe, [3, 4, 6, 7])
        self.liquidityReqAnt['hi'] = fuzz.trapmf(self.liquidityReqAnt.universe, [6, 7, 10, 10])

        # set up total risk preference as an antecedent
        x = np.arange(0, 11, 1)
        self.totalRiskPrefAnt = ctrl.Antecedent(x, 'totalRiskPrefAnt')
        self.totalRiskPrefAnt['lo'] = fuzz.trapmf(self.totalRiskPrefAnt.universe, [0, 0, 3, 4])
        self.totalRiskPrefAnt['me'] = fuzz.trapmf(self.totalRiskPrefAnt.universe, [3, 4, 6, 7])
        self.totalRiskPrefAnt['hi'] = fuzz.trapmf(self.totalRiskPrefAnt.universe, [6, 7, 10, 10])

        # set up stocks as a consequent
        x = np.arange(0, 11, 1)
        self.stocks = ctrl.Consequent(x, 'stocks')
        self.stocks['lo'] = fuzz.trapmf(self.stocks.universe, [0, 0, 3, 4])
        self.stocks['me'] = fuzz.trapmf(self.stocks.universe, [3, 4, 6, 7])
        self.stocks['hi'] = fuzz.trapmf(self.stocks.universe, [6, 7, 10, 10])

        # set up properties as a consequent
        x = np.arange(0, 11, 1)
        self.properties = ctrl.Consequent(x, 'properties')
        self.properties['lo'] = fuzz.trapmf(self.properties.universe, [0, 0, 3, 4])
        self.properties['me'] = fuzz.trapmf(self.properties.universe, [3, 4, 6, 7])
        self.properties['hi'] = fuzz.trapmf(self.properties.universe, [6, 7, 10, 10])

        # set up savings as a consequent
        x = np.arange(0, 11, 1)
        self.savings = ctrl.Consequent(x, 'savings')
        self.savings['lo'] = fuzz.trapmf(self.savings.universe, [0, 0, 3, 4])
        self.savings['me'] = fuzz.trapmf(self.savings.universe, [3, 4, 6, 7])
        self.savings['hi'] = fuzz.trapmf(self.savings.universe, [6, 7, 10, 10])

        if DEBUGLEVEL == 1:
            self.liquidityReqAnt.view()
            self.totalRiskPrefAnt.view()
            self.stocks.view()
            self.properties.view()
            self.savings.view()
        return

    # ------------------------------------------------------------------------------
    # This method:
    # (1) sets the rule base
    # (2) sets the inference engine to use the rule base
    #
    def inference(self):

        # rules 01 - 27
        # monthlyIncome & totalAssets & age -> riskAbility
        rule01 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['hi'] & self.age['me'], self.riskAbilityCon['hi'])
        rule02 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['me'] & self.age['me'], self.riskAbilityCon['me'])
        rule03 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['lo'] & self.age['me'], self.riskAbilityCon['lo'])
        rule04 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['hi'] & self.age['me'], self.riskAbilityCon['me'])
        rule05 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['me'] & self.age['me'], self.riskAbilityCon['lo'])
        rule06 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['lo'] & self.age['me'], self.riskAbilityCon['lo'])
        rule07 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['hi'] & self.age['me'], self.riskAbilityCon['lo'])
        rule08 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['me'] & self.age['me'], self.riskAbilityCon['lo'])
        rule09 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['lo'] & self.age['me'], self.riskAbilityCon['lo'])

        rule10 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['hi'] & self.age['lo'], self.riskAbilityCon['hi'])
        rule11 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['me'] & self.age['lo'], self.riskAbilityCon['hi'])
        rule12 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['lo'] & self.age['lo'], self.riskAbilityCon['me'])
        rule13 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['hi'] & self.age['lo'], self.riskAbilityCon['hi'])
        rule14 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['me'] & self.age['lo'], self.riskAbilityCon['me'])
        rule15 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['lo'] & self.age['lo'], self.riskAbilityCon['lo'])
        rule16 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['hi'] & self.age['lo'], self.riskAbilityCon['me'])
        rule17 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['me'] & self.age['lo'], self.riskAbilityCon['lo'])
        rule18 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['lo'] & self.age['lo'], self.riskAbilityCon['lo'])

        rule19 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['hi'] & self.age['hi'], self.riskAbilityCon['me'])
        rule20 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['me'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule21 = ctrl.Rule(self.monthlyIncome['hi'] & self.totalAssets['lo'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule22 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['hi'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule23 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['me'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule24 = ctrl.Rule(self.monthlyIncome['me'] & self.totalAssets['lo'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule25 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['hi'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule26 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['me'] & self.age['hi'], self.riskAbilityCon['lo'])
        rule27 = ctrl.Rule(self.monthlyIncome['lo'] & self.totalAssets['lo'] & self.age['hi'], self.riskAbilityCon['lo'])

        # set up control system for risk ability
        self.riskAbilityCS = ctrl.ControlSystem(
            [rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08, rule09, rule10,
             rule11, rule12, rule13, rule14, rule15, rule16, rule17, rule18, rule19, rule20,
             rule21, rule22, rule23, rule24, rule25, rule26, rule27
             ])
        self.riskAbilityEval = ctrl.ControlSystemSimulation(self.riskAbilityCS)

        # rules 28 - 36
        # riskAbility & takeRisk -> totalRiskPref
        rule28 = ctrl.Rule(self.riskAbilityAnt['hi'] & self.takeRisk['hi'], self.totalRiskPrefCon['hi'])
        rule29 = ctrl.Rule(self.riskAbilityAnt['hi'] & self.takeRisk['me'], self.totalRiskPrefCon['me'])
        rule30 = ctrl.Rule(self.riskAbilityAnt['hi'] & self.takeRisk['lo'], self.totalRiskPrefCon['lo'])
        rule31 = ctrl.Rule(self.riskAbilityAnt['me'] & self.takeRisk['hi'], self.totalRiskPrefCon['me'])
        rule32 = ctrl.Rule(self.riskAbilityAnt['me'] & self.takeRisk['me'], self.totalRiskPrefCon['me'])
        rule33 = ctrl.Rule(self.riskAbilityAnt['me'] & self.takeRisk['lo'], self.totalRiskPrefCon['lo'])
        rule34 = ctrl.Rule(self.riskAbilityAnt['lo'] & self.takeRisk['hi'], self.totalRiskPrefCon['lo'])
        rule35 = ctrl.Rule(self.riskAbilityAnt['lo'] & self.takeRisk['me'], self.totalRiskPrefCon['lo'])
        rule36 = ctrl.Rule(self.riskAbilityAnt['lo'] & self.takeRisk['lo'], self.totalRiskPrefCon['lo'])

        # set up control system for total risk preference
        self.totalRiskPrefCS = ctrl.ControlSystem(
            [ rule28, rule29, rule30, rule31, rule32, rule33, rule34, rule35, rule36 ])
        self.totalRiskPrefEval = ctrl.ControlSystemSimulation(self.totalRiskPrefCS)

        # rules 37 - 63
        # dependentExp & ratio_LE_TA & bigItemExp  -> liquidityReq
        rule37 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['hi'] & self.bigItemExp['hi'], self.liquidityReqCon['hi'])
        rule38 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['hi'] & self.bigItemExp['me'], self.liquidityReqCon['hi'])
        rule39 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['hi'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])
        rule40 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['hi'] & self.bigItemExp['hi'], self.liquidityReqCon['me'])
        rule41 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['hi'] & self.bigItemExp['me'], self.liquidityReqCon['hi'])
        rule42 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['hi'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])
        rule43 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['hi'] & self.bigItemExp['hi'], self.liquidityReqCon['me'])
        rule44 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['hi'] & self.bigItemExp['me'], self.liquidityReqCon['me'])
        rule45 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['hi'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])

        rule46 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['lo'] & self.bigItemExp['hi'], self.liquidityReqCon['me'])
        rule47 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['lo'] & self.bigItemExp['me'], self.liquidityReqCon['me'])
        rule48 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['lo'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])
        rule49 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['lo'] & self.bigItemExp['hi'], self.liquidityReqCon['lo'])
        rule50 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['lo'] & self.bigItemExp['me'], self.liquidityReqCon['me'])
        rule51 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['lo'] & self.bigItemExp['lo'], self.liquidityReqCon['me'])
        rule52 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['lo'] & self.bigItemExp['hi'], self.liquidityReqCon['lo'])
        rule53 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['lo'] & self.bigItemExp['me'], self.liquidityReqCon['lo'])
        rule54 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['lo'] & self.bigItemExp['lo'], self.liquidityReqCon['me'])

        rule55 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['me'] & self.bigItemExp['hi'], self.liquidityReqCon['me'])
        rule56 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['me'] & self.bigItemExp['me'], self.liquidityReqCon['hi'])
        rule57 = ctrl.Rule(self.dependentExp['lo'] & self.ratio_LE_TA['me'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])
        rule58 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['me'] & self.bigItemExp['hi'], self.liquidityReqCon['me'])
        rule59 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['me'] & self.bigItemExp['me'], self.liquidityReqCon['me'])
        rule60 = ctrl.Rule(self.dependentExp['me'] & self.ratio_LE_TA['me'] & self.bigItemExp['lo'], self.liquidityReqCon['hi'])
        rule61 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['me'] & self.bigItemExp['hi'], self.liquidityReqCon['lo'])
        rule62 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['me'] & self.bigItemExp['me'], self.liquidityReqCon['me'])
        rule63 = ctrl.Rule(self.dependentExp['hi'] & self.ratio_LE_TA['me'] & self.bigItemExp['lo'], self.liquidityReqCon['me'])

        # set up control system for liquidity requirement
        self.liquidityReqCS = ctrl.ControlSystem(
            [rule37, rule38, rule39, rule40,
             rule41, rule42, rule43, rule44, rule45, rule46, rule47, rule48, rule49, rule50,
             rule51, rule52, rule53, rule54, rule55, rule56, rule57, rule58, rule59, rule60,
             rule61, rule62, rule63
             ])
        self.liquidityReqEval = ctrl.ControlSystemSimulation(self.liquidityReqCS)

        # rules 64 - 72
        # liquidityReq & totalRiskPref -> stocks
        rule64 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['hi'], self.stocks['me'])
        rule65 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['me'], self.stocks['me'])
        rule66 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['lo'], self.stocks['lo'])
        rule67 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['hi'], self.stocks['hi'])
        rule68 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['me'], self.stocks['me'])
        rule69 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['lo'], self.stocks['lo'])
        rule70 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['hi'], self.stocks['hi'])
        rule71 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['me'], self.stocks['me'])
        rule72 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['lo'], self.stocks['lo'])

        # set up control system for stocks
        self.stocksCS = ctrl.ControlSystem(
            [ rule64, rule65, rule66, rule67, rule68, rule69, rule70, rule71, rule72 ])
        self.stocksEval = ctrl.ControlSystemSimulation(self.stocksCS)

        # rules 73 - 81
        # liquidityReq & totalRiskPref -> properties
        rule73 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['hi'], self.properties['lo'])
        rule74 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['me'], self.properties['lo'])
        rule75 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['lo'], self.properties['lo'])
        rule76 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['hi'], self.properties['me'])
        rule77 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['me'], self.properties['me'])
        rule78 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['lo'], self.properties['lo'])
        rule79 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['hi'], self.properties['me'])
        rule80 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['me'], self.properties['hi'])
        rule81 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['lo'], self.properties['me'])

        # set up control system for properties
        self.propertiesCS = ctrl.ControlSystem(
            [ rule73, rule74, rule75, rule76, rule77, rule78, rule79, rule80, rule81 ])
        self.propertiesEval = ctrl.ControlSystemSimulation(self.propertiesCS)

        # rules 82 - 90
        # liquidityReq & totalRiskPref -> savings
        rule82 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['hi'], self.savings['hi'])
        rule83 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['me'], self.savings['hi'])
        rule84 = ctrl.Rule(self.liquidityReqAnt['hi'] & self.totalRiskPrefAnt['lo'], self.savings['hi'])
        rule85 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['hi'], self.savings['me'])
        rule86 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['me'], self.savings['me'])
        rule87 = ctrl.Rule(self.liquidityReqAnt['me'] & self.totalRiskPrefAnt['lo'], self.savings['hi'])
        rule88 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['hi'], self.savings['lo'])
        rule89 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['me'], self.savings['me'])
        rule90 = ctrl.Rule(self.liquidityReqAnt['lo'] & self.totalRiskPrefAnt['lo'], self.savings['hi'])

        # set up control system for savings
        self.savingsCS = ctrl.ControlSystem(
            [ rule82, rule83, rule84, rule85, rule86, rule87, rule88, rule89, rule90 ])
        self.savingsEval = ctrl.ControlSystemSimulation(self.savingsCS)

        return

    # ------------------------------------------------------------------------------
    # This function evaluates the given data with inference engine
    # and defuzzifies the output
    # Returns stocks, properties and savings
    #
    def defuzzify(self, data):

        self.riskAbilityEval.input[self.dataheaders[1]] = data[self.dataheaders[1]]
        self.riskAbilityEval.input[self.dataheaders[2]] = data[self.dataheaders[2]]
        self.riskAbilityEval.input[self.dataheaders[3]] = data[self.dataheaders[3]]
        self.riskAbilityEval.compute()
        riskAbility = self.riskAbilityEval.output['riskAbilityCon']

        self.totalRiskPrefEval.input['riskAbilityAnt'] = riskAbility
        self.totalRiskPrefEval.input[self.dataheaders[4]]  = data[self.dataheaders[4]]
        self.totalRiskPrefEval.compute()
        totalRiskPref = self.totalRiskPrefEval.output['totalRiskPrefCon']

        self.liquidityReqEval.input[self.dataheaders[5]] = data[self.dataheaders[5]]
        self.liquidityReqEval.input[self.dataheaders[6]] = data[self.dataheaders[6]]
        self.liquidityReqEval.input[self.dataheaders[7]] = data[self.dataheaders[7]]
        self.liquidityReqEval.compute()
        liquidityReq = self.liquidityReqEval.output['liquidityReqCon']

        self.stocksEval.input['totalRiskPrefAnt'] = totalRiskPref
        self.stocksEval.input['liquidityReqAnt']  = liquidityReq
        self.stocksEval.compute()
        stocks = self.stocksEval.output['stocks']

        self.propertiesEval.input['totalRiskPrefAnt'] = totalRiskPref
        self.propertiesEval.input['liquidityReqAnt'] = liquidityReq
        self.propertiesEval.compute()
        properties = self.propertiesEval.output['properties']

        self.savingsEval.input['totalRiskPrefAnt'] = totalRiskPref
        self.savingsEval.input['liquidityReqAnt'] = liquidityReq
        self.savingsEval.compute()
        savings = self.savingsEval.output['savings']

        total = (stocks + properties + savings)/100
        consequents = [[]]
        consequents[0] = [riskAbility, totalRiskPref, liquidityReq]
        consequents.append([stocks, properties, savings])
        consequents.append([stocks/total, properties/total, savings/total])

        # return riskAbility, totalRiskPref, liquidityReq, stocks, properties, savings
        return consequents

