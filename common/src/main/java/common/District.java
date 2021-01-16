package common;

import com.fasterxml.jackson.annotation.JsonClassDescription;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;

@JsonClassDescription
public class District {
    private String name;
    private int totalUsers;
    private int totalInfected;
    private int meetInfected;
    private Map<String, Integer> usersPerLocation;

    public District() {
    }

    /*
     * public District(String name, int totalUsers, int totalInfected, int
     * meetInfected, Map<String, Integer> usersPerLocation){ this.name = name;
     * this.totalUsers = totalUsers; this.totalInfected = totalInfected;
     * this.meetInfected = meetInfected; this.usersPerLocation = usersPerLocation; }
     * 
     */
    @JsonCreator
    public District(@JsonProperty("name") String name, @JsonProperty("totalUsers") int totalUsers,
            @JsonProperty("totalInfected") int totalInfected, @JsonProperty("meetInfected") int meetInfected,
            @JsonProperty("usersPerLocation") Map<String, Integer> usersPerLocation) {
        this.name = name;
        this.totalUsers = totalUsers;
        this.totalInfected = totalInfected;
        this.meetInfected = meetInfected;
        this.usersPerLocation = usersPerLocation;
    }

    @JsonProperty
    public String getName() {
        return name;
    }

    @JsonProperty
    public int getTotalUsers() {
        return totalUsers;
    }

    @JsonProperty
    public int getTotalInfected() {
        return totalInfected;
    }

    @JsonProperty
    public int getMeetInfected() {
        return meetInfected;
    }

    @JsonProperty
    public Map<String, Integer> getUsersPerLocation() {
        return usersPerLocation;
    }

    @JsonProperty
    public void setName(String name) {
        this.name = name;
    }

    @JsonProperty
    public void setTotalUsers(Integer u) {
        this.totalUsers = u;
    }

    @JsonProperty
    public void setTotalInfected(Integer u) {
        this.totalInfected = u;
    }

    @JsonProperty
    public void setMeetInfected(Integer u) {
        this.meetInfected = u;
    }

    @JsonProperty
    public void setUsersPerLocation(Map<String, Integer> usersPerLocation) {
        this.usersPerLocation = usersPerLocation;
    }

    private String convertToString(Map<String, Integer> map) {
        StringBuilder mapAsString = new StringBuilder("{");

        for (String key : map.keySet()) {
            mapAsString.append(key + "= " + map.get(key) + ", ");
        }
        if (!map.isEmpty()) {
            mapAsString.delete(mapAsString.length() - 2, mapAsString.length()).append("}");
        } else {
            mapAsString.append("}");
        }

        return mapAsString.toString();
    }

    @Override
    public String toString() {
        return "District{" + "name='" + name + '\'' + ", totalUsers='" + totalUsers + '\'' + ", totalInfected=' "
                + totalInfected + '\'' + ", meetInfected=' " + meetInfected + '\'' + ", usersPerLocation=' "
                + convertToString(usersPerLocation) + '\'' + '}';
    }

    public String toJson() {
        try {
            ObjectMapper om = new ObjectMapper();
            return om.writeValueAsString(this);
        } catch (Exception e) {}
        return null;
    }

    private boolean verifyLocation(Map<String,Integer> usersLocation, District district){

        if (usersLocation.size() != district.getUsersPerLocation().size()) {
            return false;
        }

        return usersLocation.entrySet().stream()
                .allMatch(e -> e.getValue().equals(district.getUsersPerLocation()
                        .get(e.getKey())));
    }

    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o)
            return true;
        // null check
        if (o == null)
            return false;
        // type check and cast
        if (getClass() != o.getClass())
            return false;
        District district = (District) o;
        // field comparison
        return (this.getName().equals(district.getName()) &&
                this.getTotalUsers()== district.getTotalUsers() &&
                this.getMeetInfected() == district.getMeetInfected() &&
                district.getTotalInfected() == district.getTotalInfected() &&
                verifyLocation(this.getUsersPerLocation(), district));
    }
}
