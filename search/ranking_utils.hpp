#pragma once

#include "search/common.hpp"
#include "search/model.hpp"
#include "search/query_params.hpp"

#include "indexer/search_delimiters.hpp"
#include "indexer/search_string_utils.hpp"

#include "base/levenshtein_dfa.hpp"
#include "base/stl_helpers.hpp"
#include "base/string_utils.hpp"

#include <algorithm>
#include <cstddef>
#include <limits>
#include <string>
#include <vector>

class CategoriesHolder;

namespace feature
{
class TypesHolder;
}

namespace search
{
class QueryParams;
class TokenSlice;

class CategoriesInfo
{
public:
  CategoriesInfo(feature::TypesHolder const & holder, TokenSlice const & tokens,
                 Locales const & locales, CategoriesHolder const & categories);

  // Returns true when all tokens correspond to categories in
  // |holder|.
  bool IsPureCategories() const { return m_pureCategories; }

  // Returns true when all tokens are categories tokens but none of
  // them correspond to categories in |holder|.
  bool IsFalseCategories() const { return m_falseCategories; }

private:
  bool m_pureCategories = false;
  bool m_falseCategories = false;
};

struct ErrorsMade
{
  static size_t constexpr kInfiniteErrors = std::numeric_limits<size_t>::max();

  ErrorsMade() = default;
  explicit ErrorsMade(size_t errorsMade) : m_errorsMade(errorsMade) {}

  bool IsValid() const { return m_errorsMade != kInfiniteErrors; }

  template <typename Fn>
  static ErrorsMade Combine(ErrorsMade const & lhs, ErrorsMade const & rhs, Fn && fn)
  {
    if (!lhs.IsValid())
      return rhs;
    if (!rhs.IsValid())
      return lhs;
    return ErrorsMade(fn(lhs.m_errorsMade, rhs.m_errorsMade));
  }

  static ErrorsMade Min(ErrorsMade const & lhs, ErrorsMade const & rhs)
  {
    return Combine(lhs, rhs, [](size_t u, size_t v) { return std::min(u, v); });
  }

  static ErrorsMade Max(ErrorsMade const & lhs, ErrorsMade const & rhs)
  {
    return Combine(lhs, rhs, [](size_t u, size_t v) { return std::max(u, v); });
  }

  friend ErrorsMade operator+(ErrorsMade const & lhs, ErrorsMade const & rhs)
  {
    return Combine(lhs, rhs, [](size_t u, size_t v) { return u + v; });
  }

  ErrorsMade & operator+=(ErrorsMade const & rhs)
  {
    *this = *this + rhs;
    return *this;
  }

  bool operator==(ErrorsMade const & rhs) const { return m_errorsMade == rhs.m_errorsMade; }

  size_t m_errorsMade = kInfiniteErrors;
};

std::string DebugPrint(ErrorsMade const & errorsMade);

namespace impl
{
// Returns the minimum number of errors needed to match |text| with |token|.
// If it's not possible in accordance with GetMaxErrorsForToken(|text|), returns kInfiniteErrors.
ErrorsMade GetErrorsMade(QueryParams::Token const & token, strings::UniString const & text);
ErrorsMade GetPrefixErrorsMade(QueryParams::Token const & token, strings::UniString const & text);
}  // namespace impl

// The order and numeric values are important here.  Please, check all
// use-cases before changing this enum.
enum NameScore
{
  NAME_SCORE_ZERO = 0,
  NAME_SCORE_SUBSTRING = 1,
  NAME_SCORE_PREFIX = 2,
  NAME_SCORE_FULL_MATCH = 3,

  NAME_SCORE_COUNT
};

struct NameScores
{
  NameScores() = default;
  NameScores(NameScore nameScore, ErrorsMade const & errorsMade, bool isAltOrOldName, size_t matchedLength)
    : m_nameScore(nameScore), m_errorsMade(errorsMade), m_isAltOrOldName(isAltOrOldName), m_matchedLength(matchedLength)
  {
  }

  void UpdateIfBetter(NameScores const & rhs)
  {
    auto const newNameScoreIsBetter = m_nameScore < rhs.m_nameScore;
    auto const nameScoresAreEqual = m_nameScore == rhs.m_nameScore;
    auto const newLanguageIsBetter = m_isAltOrOldName && !rhs.m_isAltOrOldName;
    auto const languagesAreEqual = m_isAltOrOldName == rhs.m_isAltOrOldName;
    auto const newMatchedLengthIsBetter = m_matchedLength < rhs.m_matchedLength;
    auto const matchedLengthAreEqual = m_matchedLength == rhs.m_matchedLength;

    if (newNameScoreIsBetter ||
       (nameScoresAreEqual && newMatchedLengthIsBetter) ||
       (nameScoresAreEqual && matchedLengthAreEqual && newLanguageIsBetter))
    {
      m_nameScore = rhs.m_nameScore;
      m_errorsMade = rhs.m_errorsMade;
      m_isAltOrOldName = rhs.m_isAltOrOldName;
      m_matchedLength = rhs.m_matchedLength;
      return;
    }
    if (nameScoresAreEqual && matchedLengthAreEqual && languagesAreEqual)
      m_errorsMade = ErrorsMade::Min(m_errorsMade, rhs.m_errorsMade);
  }

  bool operator==(NameScores const & rhs)
  {
    return m_nameScore == rhs.m_nameScore && m_errorsMade == rhs.m_errorsMade &&
           m_isAltOrOldName == rhs.m_isAltOrOldName;
  }

  NameScore m_nameScore = NAME_SCORE_ZERO;
  ErrorsMade m_errorsMade;
  bool m_isAltOrOldName = false;
  size_t m_matchedLength = 0;
};

// Returns true when |s| is a stop-word and may be removed from a query.
bool IsStopWord(strings::UniString const & s);

// Normalizes, simplifies and splits string, removes stop-words.
void PrepareStringForMatching(std::string const & name, std::vector<strings::UniString> & tokens);

template <typename Slice>
NameScores GetNameScores(std::vector<strings::UniString> const & tokens, uint8_t lang,
                         Slice const & slice)
{
  if (slice.Empty())
    return {};

  size_t const n = tokens.size();
  size_t const m = slice.Size();

  bool const lastTokenIsPrefix = slice.IsPrefix(m - 1);

  NameScores scores;
  for (size_t offset = 0; offset + m <= n; ++offset)
  {
    ErrorsMade totalErrorsMade;
    size_t matchedLength = 0;
    bool match = true;
    for (size_t i = 0; i < m - 1 && match; ++i)
    {
      auto errorsMade = impl::GetErrorsMade(slice.Get(i), tokens[offset + i]);
      match = match && errorsMade.IsValid();
      totalErrorsMade += errorsMade;
      // The match length is preserved only if match is true below
      matchedLength += slice.Get(i).GetOriginal().size();
    }

    if (!match)
      continue;

    auto const prefixErrorsMade =
        lastTokenIsPrefix ? impl::GetPrefixErrorsMade(slice.Get(m - 1), tokens[offset + m - 1])
                          : ErrorsMade{};
    auto const fullErrorsMade = impl::GetErrorsMade(slice.Get(m - 1), tokens[offset + m - 1]);
    if (!fullErrorsMade.IsValid() && !(prefixErrorsMade.IsValid() && lastTokenIsPrefix))
      continue;

    // If error count is valid, then the match length should increase
    matchedLength += slice.Get(m-1).GetOriginal().size();

    LOG(LDEBUG, ("BJN Match length", matchedLength, "from", tokens, "into", slice));

    auto const isAltOrOldName =
        lang == StringUtf8Multilang::kAltNameCode || lang == StringUtf8Multilang::kOldNameCode;
    if (m == n && fullErrorsMade.IsValid())
    {
      scores.m_nameScore = NAME_SCORE_FULL_MATCH;
      scores.m_errorsMade = totalErrorsMade + fullErrorsMade;
      scores.m_isAltOrOldName = isAltOrOldName;
      scores.m_matchedLength = matchedLength;
      return scores;
    }

    auto const newErrors =
        lastTokenIsPrefix ? ErrorsMade::Min(fullErrorsMade, prefixErrorsMade) : fullErrorsMade;

    if (offset == 0)
    {
      scores.UpdateIfBetter(
          NameScores(NAME_SCORE_PREFIX, totalErrorsMade + newErrors, isAltOrOldName, matchedLength));
    }

    scores.UpdateIfBetter(
        NameScores(NAME_SCORE_SUBSTRING, totalErrorsMade + newErrors, isAltOrOldName, matchedLength));
  }
  return scores;
}

template <typename Slice>
NameScores GetNameScores(std::string const & name, uint8_t lang, Slice const & slice)
{
  std::vector<strings::UniString> tokens;
  SplitUniString(NormalizeAndSimplifyString(name), base::MakeBackInsertFunctor(tokens),
                 Delimiters());
  return GetNameScores(tokens, lang, slice);
}

std::string DebugPrint(NameScore score);
std::string DebugPrint(NameScores scores);
}  // namespace search
