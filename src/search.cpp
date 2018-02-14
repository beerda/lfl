/*
 * File name: search.cpp
 * Date:      2013/08/21 10:49
 * Author:    Michal Burda
 */


#include <Rcpp.h>
#include <common.h>
#include "search/Search.h"
#include "search/BasicExtension.h"
#include "search/TrieExtension.h"

#include <iostream>

using namespace Rcpp;
using namespace std;
using namespace lfl::search;


// [[Rcpp::export(name=".search")]]
List search(NumericMatrix rcppData, List config) {
    List result;
    try {
        SearchConfig searchConfig;

        NumericVector rcppVars = config["vars"];
        NumericVector rcppLhs = config["lhs"];
        NumericVector rcppRhs = config["rhs"];

        IdVector vars(rcppVars.begin(), rcppVars.end());
        IdVector lhs(rcppLhs.begin(), rcppLhs.end());
        IdVector rhs(rcppRhs.begin(), rcppRhs.end());

        string tnorm = Rcpp::as<std::string>(config["tnorm"]);
        string best = Rcpp::as<std::string>(config["best"]);

        searchConfig.setNumThreads(config["numThreads"]);
        searchConfig.setVariables(vars);
        searchConfig.setTNorm(tnorm[0]);                         // 'm' (minimum), 'l' (lukasiewicz) or 'p' (product)
        searchConfig.setLhs(lhs);                      // vector of column indexes
        searchConfig.setRhs(rhs);                      // vector of column indexes
        searchConfig.setMinSupport(config["minSupport"]);        // [0, 1]
        searchConfig.setMinConfidence(config["minConfidence"]);  // [0, 1]
        searchConfig.setMaxConfidence(config["maxConfidence"]);  // [0, 1]
        searchConfig.setMaxLength(config["maxLength"]);          // 0, 1, 2, ...
        searchConfig.setRuleNumber(config["n"]);                 // integer > 0
        searchConfig.setBestBy(best[0]);                         // 'c' (confidence)
        searchConfig.setRowCount(rcppData.nrow());
        searchConfig.setColCount(rcppData.ncol());

        //bool trieEnabled = (searchConfig.getMaxConfidence() < 1);
        bool trieEnabled = config["trie"];

        BasicExtension* basicExtension = 0;
        TrieExtension* trieExtension = 0;
        Search* search = 0;
        basicExtension = new BasicExtension();
        if (trieEnabled) {
            trieExtension = new TrieExtension(basicExtension);
            search = new Search(searchConfig, trieExtension);
        } else {
            search = new Search(searchConfig, basicExtension);
        }

        for (int row = 0; row < rcppData.nrow(); row++) {
            for (int col = 0; col < rcppData.ncol(); col++) {
                search->getData().setValue(row, col, rcppData(row, col));
            }
        }

        search->run();

        vector<IdVector> rules;
        vector<vector<double>> stats;
        AbstractStorage* storage = basicExtension->getStorage();

        while (!storage->empty()) {
            Rule* r = storage->popRule();
            IdVector cond;
            cond.push_back(r->getRhs());
            std::copy(r->getLhs().begin(), r->getLhs().end(), back_inserter(cond));

            rules.push_back(cond);

            vector<double> stat;
            Statistics s = r->getStatistics();
            for (int i = 0; i < INDEX_END; i++) {
                stat.push_back(s.get(i));
            }
            stats.push_back(stat);
            delete r;
        }

        // rules are obtained from worst to best, so reverse the order before return
        std::reverse(rules.begin(), rules.end());
        std::reverse(stats.begin(), stats.end());

        result = List::create(_["rules"] = rules, _["statistics"] = stats);
        //NumericVector result(1);

        delete search;
        delete basicExtension;

        if (trieExtension) {
            delete trieExtension;
        }

    } catch (std::exception& e) {
        //cerr << e.what() << endl;
    }

    return result;
}


