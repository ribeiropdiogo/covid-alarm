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
}
