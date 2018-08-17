/*
 * File name: reduce.cpp
 * Date:      2015/01/09 10:49
 * Author:    Michal Burda
 */


#include <Rcpp.h>
#include <common.h>
#include "reduce/Reduce.h"

using namespace Rcpp;
using namespace std;
using namespace lfl::reduce;


// [[Rcpp::export(name=".reduce")]]
NumericVector reduce(List config) {
    NumericVector result;
    try {
        ReduceConfig reduceConfig;

        NumericMatrix rcppData = config["data"];
        NumericVector ratio = config["ratio"];
        string tnorm = Rcpp::as<std::string>(config["tnorm"]);
        string tconorm = Rcpp::as<std::string>(config["tconorm"]);
        List rcppRules = config["rules"];
        NumericVector rcppLhsSupport = config["lhsSupport"];

        reduceConfig.setNumThreads(config["numThreads"]);
        reduceConfig.setRatio(ratio[0]);
        reduceConfig.setTNorm(tnorm[0]);
        reduceConfig.setTConorm(tconorm[0]);
        reduceConfig.setRowCount(rcppData.nrow());
        reduceConfig.setColCount(rcppData.ncol());

        Reduce* reduce = new Reduce(reduceConfig);

        for (R_len_t row = 0; row < rcppData.nrow(); row++) {
            for (R_len_t col = 0; col < rcppData.ncol(); col++) {
                reduce->getData().setValue(row, col, rcppData(row, col));
            }
        }

        for (R_len_t id = 0; id < rcppRules.size(); id++) {
            NumericVector rcppRule(rcppRules[id]);
            Rule* rule = reduce->createRule(id);
            rule->setPotential(rcppLhsSupport[id], 0L);
            rule->setConsequent(rcppRule[0]);

            IdSet& ante = rule->getAntecedent();
            for (R_len_t i = 1; i < rcppRule.length(); i++) {
                ante.insert(rcppRule[i]);
            }

            reduce->getInputRules().push_back(rule);
        }

        reduce->run();

        result = NumericVector(reduce->getOutputRules().size());
        size_t i = 0;
        for (RuleVector::iterator it = reduce->getOutputRules().begin();
                it != reduce->getOutputRules().end(); it++) {
            result[i++] = (*it)->getId();
        }

        delete reduce;
    } catch (std::exception& e) {
        //cerr << e.what() << endl;
    }

    return result;
}
