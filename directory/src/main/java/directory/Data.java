package directory;

import common.District;

import java.util.*;
import java.util.stream.Collectors;

public class Data {
    private static List<District> infoPerDistrict = new ArrayList<>();

    static {

        Map<String, Integer> locationsBraga = new HashMap<String, Integer>();
        locationsBraga.put("Guimaraes", 100);
        locationsBraga.put("Braga", 160);
        locationsBraga.put("Vizela", 17);
        locationsBraga.put("VilaVerde", 30);
        locationsBraga.put("Barcelos", 34);
        locationsBraga.put("PovoaLanhoso", 34);

        Map<String, Integer> locationsLisboa = new HashMap<String, Integer>();
        locationsLisboa.put("Oeiras", 115);
        locationsLisboa.put("Lisboa", 400);
        locationsLisboa.put("Cascais", 200);
        locationsLisboa.put("Loures", 300);
        locationsLisboa.put("Amadora", 45);
        locationsLisboa.put("Odivelas", 45);
        locationsLisboa.put("Sintra", 65);
        locationsLisboa.put("Mafra", 80);



        Map<String, Integer> locationsPorto = new HashMap<String, Integer>();
        locationsPorto.put("Porto", 350);
        locationsPorto.put("StoTirso", 10);
        locationsPorto.put("Gaia", 17);
        locationsPorto.put("Maia", 30);
        locationsPorto.put("Matosinhos", 45);
        locationsPorto.put("Gondomar", 45);


        Map<String, Integer> locationsAveiro = new HashMap<String, Integer>();
        locationsAveiro.put("Agueda", 150);
        locationsAveiro.put("Ovar", 100);
        locationsAveiro.put("Espinho", 127);
        locationsAveiro.put("Aveiro", 90);
        locationsAveiro.put("Estarreja", 45);

        Map<String, Integer> locationsLeiria = new HashMap<String, Integer>();
        locationsLeiria.put("Leiria", 90);
        locationsLeiria.put("Ansiao", 100);
        locationsLeiria.put("Batalha", 127);
        locationsLeiria.put("Alcobaca", 90);
        locationsLeiria.put("Peniche", 45);
        locationsLeiria.put("Obidos", 30);

        Map<String, Integer> locationsCoimbra = new HashMap<String, Integer>();
        locationsCoimbra.put("Coimbra", 75);
        locationsCoimbra.put("Penacova", 50);
        locationsCoimbra.put("Mira", 17);
        locationsCoimbra.put("Penela", 29);
        locationsCoimbra.put("Soure", 35);


        District d1 = new District("Lisboa", 1000, 500,600, locationsLisboa);
        District d3 = new District("Porto", 1000, 400,234, locationsPorto);
        District d4 = new District("Aveiro", 1000, 350,209, locationsAveiro);
        District d5 = new District("Leiria", 1000, 450,202, locationsLeiria);
        District d6 = new District("Coimbra", 1000, 200,200, locationsCoimbra);
        District d2 = new District("Braga", 1000, 300,200, locationsBraga);

        infoPerDistrict.add(d1);
        infoPerDistrict.add(d2);
        infoPerDistrict.add(d3);
        infoPerDistrict.add(d4);
        infoPerDistrict.add(d5);
        infoPerDistrict.add(d6);

    }


    //Adds a district into the districts' list if it doesn't exists
    public static District addDistrict(District district){
        for(District d: infoPerDistrict) {
            if(d.getName().equals(district.getName())){
                System.out.println("District already exists");
                return null;
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
    public static void removeDistrict(String name){
        for(District d: infoPerDistrict) {
            if (d.getName().equals(name)) {
                infoPerDistrict.remove(d);
            }
        }
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

    private static float racioInfectedUsers(District district) {
        float res = ((float)district.getTotalInfected()/(float)district.getTotalUsers());
        return res;
    }

    //Collects the 5 Districts that have the higher ratio between infected and totalUsers
    public static List<District> getRacioMostInfected() {
        Map<District, Float> top5 = new HashMap<District, Float>();
        float racio = 0;

        for(District district : infoPerDistrict) {
            racio = racioInfectedUsers(district);
            top5.put(district, racio);
        }



        return top5.entrySet().stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(5).map(Map.Entry::getKey)
                .collect(Collectors.toList());

    }

    //Collects the name of the 5 locations with more people together at the same time
    public static List<String> top5CrowdedLocation(){
        List<Map.Entry<String, Integer>> top5Locations = new ArrayList<>();

        List<Map.Entry<String, Integer>> top5LocationsPerRegion = new ArrayList<>();

        for(District district : infoPerDistrict) {
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



    public static float meanMeetInfected(){
        float totalUsers = 0;
        float totalMeetInfected = 0;

        for(District district : infoPerDistrict) {
            totalUsers += (float)district.getTotalUsers();
            totalMeetInfected += (float)district.getMeetInfected();
        }

        return (totalMeetInfected/totalUsers);
    }
}
