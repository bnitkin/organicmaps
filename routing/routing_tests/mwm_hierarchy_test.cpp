#include "testing/testing.hpp"

#include "routing/mwm_hierarchy_handler.hpp"
#include "storage/country_parent_getter.hpp"


using namespace routing;

UNIT_TEST(CountryParentGetter_Smoke)
{
  storage::CountryParentGetter getter;
  TEST_EQUAL(getter("Belarus_Hrodna Region"), "Belarus", ());
  TEST_EQUAL(getter("Russia_Arkhangelsk Oblast_Central"), "Russian Federation", ());
  TEST_EQUAL(getter("Crimea"), "", ());
}

namespace
{

std::shared_ptr<NumMwmIds> CreateNumMwmIds(storage::CountryParentGetter const & getter)
{
  auto mwmIDs = std::make_shared<NumMwmIds>();
  getter.GetStorageForTesting().ForEachCountryFile(
      [&mwmIDs](platform::CountryFile const & file) { mwmIDs->RegisterFile(file); });
  return mwmIDs;
}

uint16_t GetCountryID(std::shared_ptr<NumMwmIds> mwmIDs, std::string const & mwmName)
{
  return mwmIDs->GetId(platform::CountryFile(mwmName));
}

} // namespace

UNIT_TEST(MwmHierarchyHandler_Smoke)
{
  storage::CountryParentGetter getter;
  auto mwmIDs = CreateNumMwmIds(getter);
  routing::MwmHierarchyHandler handler(mwmIDs, getter);

  TEST(!handler.HasCrossBorderPenalty(GetCountryID(mwmIDs, "Belarus_Maglieu Region"),
                                      GetCountryID(mwmIDs, "Belarus_Vitebsk Region")), ());
  TEST(handler.HasCrossBorderPenalty(GetCountryID(mwmIDs, "Belarus_Hrodna Region"),
                                     GetCountryID(mwmIDs, "Lithuania_East")), ());
  TEST(!handler.HasCrossBorderPenalty(GetCountryID(mwmIDs, "Belarus_Maglieu Region"),
                                      GetCountryID(mwmIDs, "Russia_Smolensk Oblast")), ());
  TEST(handler.HasCrossBorderPenalty(GetCountryID(mwmIDs, "Ukraine_Kherson Oblast"),
                                     GetCountryID(mwmIDs, "Crimea")), ());
  TEST(!handler.HasCrossBorderPenalty(GetCountryID(mwmIDs, "Denmark_Region Zealand"),
                                      GetCountryID(mwmIDs, "Denmark_Region of Southern Denmark")), ());
}
