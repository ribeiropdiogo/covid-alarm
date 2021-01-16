package directory;

import common.District;

import java.util.*;
import java.util.stream.Collectors;

public class Data {
    private static List<District> infoPerDistrict = new ArrayList<>();


    //Adds a district into the districts' list if it doesn't exists
    public static District addDistrict(District district){
        for(District d: infoPerDistrict) {
            if(d.getName().equals(district.getName())){
                System.out.println("District already exists");
                return district;
            }
        }
        infoPerDistrict.add(district);

        return district;
    }

    public static List<District> allDistricts(){
        return infoPerDistrict;
    }

    //Updates info about some district
    public static District updateDistrictExceptName(String name, District district) {
        for (District d : infoPerDistrict) {
            if (d.getName().equals(name)) {
                d.setTotalUsers(district.getTotalUsers());
                d.setTotalInfected(district.getTotalInfected());
                d.setMeetInfected(district.getMeetInfected());
                d.setUsersPerLocation(district.getUsersPerLocation());
                return d;
            }
        }

        return null;
    }

    //Removes a district
    public static boolean removeDistrict(String name){
        for(District d: infoPerDistrict) {
            if (d.getName().equals(name)) {
                infoPerDistrict.remove(d);
                return true;
            }
        }
        return false;
    }

    //Retrieves the number users by district
    public static Integer getUsersPerDistrict(String districtName){
        int result = 0;

        for (District district: infoPerDistrict) {
            if (district.getName().equals(districtName)) {
                result = district.getTotalUsers();
                return result;
            }
        }
        return result;
    }


    //Retrieves the number of infected users by district
    public static int getInfectedPerDistrict(String districtName) {
        int result = 0;

        for (District district: infoPerDistrict) {
            if (district.getName().equals(districtName)) {
                result = district.getTotalInfected();
                return result;
            }
        }

        return result;
    }

    //Calculates the racio between totalInfected/allUsers by district
    private static float racioInfectedUsers(District district) {
        float res = ((float)district.getTotalInfected()/(float)district.getTotalUsers());
        return res;
    }

    public static Map<String, Float> getRacioMostInfected(List<District> info) {
        Map<String, Float> top5 = new HashMap<String, Float>();
        float racio = 0;

        for(District district : info) {
            racio = racioInfectedUsers(district);
            top5.put(district.getName(), racio);
        }

        return top5.entrySet().stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(5).collect(Collectors.toMap(e-> e.getKey(), e->e.getValue()));
    }

    //Collects the 5 Districts that have the higher ratio between infected and totalUsers
    public static Map<String, Float> getRacioMostInfected() {
        return getRacioMostInfected(infoPerDistrict);
    }

    //Collects the name of the 5 locations with more people together at the same time
    public static List<String> top5CrowdedLocation(List<District> info){
        List<Map.Entry<String, Integer>> top5Locations = new ArrayList<>();

        List<Map.Entry<String, Integer>> top5LocationsPerRegion = new ArrayList<>();

        for(District district : info) {
            top5LocationsPerRegion = district.getUsersPerLocation().entrySet().stream().
                    sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                    .limit(5)
                    .collect(Collectors.toList());
            top5Locations.addAll(top5LocationsPerRegion);
        }

        return top5Locations.stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(5)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    //Returns the names of the top 5 locations more crowded
    public static List<String> top5CrowdedLocation(){
        return top5CrowdedLocation(infoPerDistrict);
    }

    //Returns the mean of people that has been with an infected user in the same location
    public static float meanMeetInfected(List<District> info){
        float totalUsers = 0;
        float totalMeetInfected = 0;

        for(District district : info) {
            totalUsers += (float)district.getTotalUsers();
            totalMeetInfected += (float)district.getMeetInfected();
        }
        return (totalMeetInfected/totalUsers);
    }

    //mean of users that have been together at the same location
    public static float meanMeetInfected(){
        return meanMeetInfected(infoPerDistrict);
    }
}
