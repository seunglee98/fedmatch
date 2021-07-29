
#include <Rcpp.h>
#include <boost/tokenizer.hpp>
#include <boost/algorithm/string.hpp>
#include <algorithm>
#include <string>
#include <unordered_map>
#ifdef _OPENMP
  #include <omp.h>
#endif
#include <vector>

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(openmp)]]


std::vector<std::string> vec_intersect(const std::vector<std::string> &v1,
                                       const std::vector<std::string> &v2){
    std::vector<std::string> v3;

    std::set_intersection(v1.begin(),v1.end(),
                          v2.begin(),v2.end(),
                          std::back_inserter(v3));
    return v3;
}


std::vector<std::string> vec_union(const std::vector<std::string> &v1,
                                   const std::vector<std::string> &v2){
    std::vector<std::string> v3;

    std::set_union(v1.begin(),v1.end(),
                   v2.begin(),v2.end(),
                   std::back_inserter(v3));

    std::vector<std::string>::iterator it;
    it = std::unique(v3.begin(), v3.end());
    v3.resize(std::distance(v3.begin(),it));

    return v3;
}

double weight_lookup_sum(const std::vector<std::string>& words,
                         const std::unordered_map<std::string, double>& weight_lookup)
{
    if(words.empty()){
        return 0;
    }

    auto sum_weights = [&](double cumulative_sum, const std::string& word) {
        return cumulative_sum + weight_lookup.at(word);
    };

    double wgt_intersection = std::accumulate(
                    std::next(words.cbegin()),
                    words.cend(),
                    weight_lookup.at(words[0]),
                    sum_weights);

    return wgt_intersection;
}

double jaccard_score(const std::vector<std::string>& words_x,
                     const std::vector<std::string>& words_y,
                     const std::unordered_map<std::string, double>& weight_lookup){
    auto word_intersection = vec_intersect(words_x, words_y);
    if(word_intersection.empty()){
        return 0;
    }
    auto word_union = vec_union(words_x, words_y);

    //auto Rprint = [](const std::string& s) { Rcpp::Rcout << " " << s; };
    //Rcpp::Rcout << "\n Intersect:";
    //Std::for_each(word_intersection.cbegin(), word_intersection.cend(), Rprint);
    //Rcpp::Rcout << "\n Union:";
    //Std::for_each(word_union.cbegin(), word_union.cend(), Rprint);

    return weight_lookup_sum(word_intersection, weight_lookup) /
        weight_lookup_sum(word_union, weight_lookup);
}

// the following line is not actually a comment, it is interpreted by Rcpp
// [[Rcpp::export]]
Rcpp::List wgt_jaccard(
        Rcpp::StringVector corpus,
        Rcpp::NumericVector wgts,
        Rcpp::StringVector x,
        Rcpp::StringVector y,
        int nthreads
        ) {

//    Rcpp::Rcout << "\n\n        nthreads: " << nthreads << "\n";
    const bool DEBUG = false;

    #ifdef _OPENMP
        omp_set_num_threads(nthreads);
    #endif

    using tokenizer = boost::tokenizer<boost::char_separator<char> >;
    const boost::char_separator<char> sep(" ");

    std::unordered_map<std::string, double> weight_lookup;
    for(int i=0; i < corpus.size(); i++){
        const auto corpus_element = static_cast<std::string>(corpus(i));
        weight_lookup[corpus_element] = wgts[i];
    }

    std::vector<std::vector<std::string> > y_tokenized(y.size());

    #pragma omp parallel for schedule(dynamic)
    for(int i=0; i < y.size(); i++){
        tokenizer tokens(y(i), sep);
        // create vector to store words
        auto y_token_vector = std::vector<std::string>(tokens.begin(), tokens.end());
        std::sort(y_token_vector.begin(), y_token_vector.end());
        y_tokenized[i] = y_token_vector;
    }

    //auto Rprint = [](const std::string& s) { Rcpp::Rcout << " " << s; };

    std::vector<unsigned int> match_indices(x.size());
    std::vector<double> match_similarity(x.size());

    #pragma omp parallel for schedule(dynamic)
    for(int i=0; i < x.size(); i++){
        // iterate over company names in X
        if (DEBUG){
            Rcpp::Rcout << "i is: " << i << ", the element value is: " << x(i);
            Rcpp::Rcout << "\n";
        }

        // split the company name
        tokenizer tokens(x(i), sep);
        // create vector to store words
        auto x_tok_vec = std::vector<std::string>(tokens.begin(), tokens.end());
        std::sort(x_tok_vec.begin(), x_tok_vec.end());

        std::vector<double> y_scores(y.size());
        for(int j=0; j < y.size(); j++){
        //for(auto& y_tok_vec : y_tokenized) {

            auto xy_score = jaccard_score(x_tok_vec, y_tokenized[j], weight_lookup);
            y_scores[j] = xy_score;

            /*
            if (DEBUG){
                Rcpp::Rcout << "   j is: " << j << ", the element value is: ";
                std::for_each(y_tokenized[j].cbegin(), y_tokenized[j].cend(), Rprint);
                Rcpp::Rcout << "\n";
                Rcpp::Rcout << "        Intersection:\n          "; std::for_each(word_intersection.cbegin(), word_intersection.cend(), Rprint);
                Rcpp::Rcout << "\n\n        Union:\n          ";
                std::for_each(word_union.cbegin(), word_union.cend(), Rprint);

                Rcpp::Rcout << "\n\n        Score: " << xy_score << "\n";
                Rcpp::Rcout << "\n";
                Rcpp::Rcout << "\n";
            }
            */
        }
        const auto result = std::max_element(y_scores.cbegin(), y_scores.cend());
        const auto max_element_index = std::distance(y_scores.cbegin(), result);

        match_indices[i] = max_element_index;
        match_similarity[i] = y_scores[max_element_index];
        if (DEBUG){
            Rcpp::Rcout << "max element at: " << std::distance(y_scores.cbegin(), result) << '\n';
        }
    }

    Rcpp::NumericVector ret_indexes = Rcpp::NumericVector(
            match_indices.cbegin(),
            match_indices.cend());

    Rcpp::NumericVector ret_similarity = Rcpp::NumericVector(
            match_similarity.cbegin(),
            match_similarity.cend());

    return Rcpp::List::create(Rcpp::Named("index") = ret_indexes,
                              Rcpp::Named("similarity") = ret_similarity);
}
