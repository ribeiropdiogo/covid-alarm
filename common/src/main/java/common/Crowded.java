package common;

import com.fasterxml.jackson.annotation.JsonClassDescription;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

@JsonClassDescription
public class Crowded {

    private String nameLocation;
    private int nUsers;

    public Crowded(){}

    @JsonCreator
    public Crowded(@JsonProperty("name") String nameLocation, @JsonProperty("totalUsers") int nUsers) {
        this.nameLocation = nameLocation;
        this.nUsers = nUsers;
    }

    @JsonProperty
    public String getNameLocation() {
        return nameLocation;
    }

    @JsonProperty
    public int getNUsers() {
        return nUsers;
    }

    @JsonProperty
    public void setNameLocation(String name) {
        this.nameLocation = name;
    }

    @JsonProperty
    public void setnUsers(int nUser){
        this.nUsers = nUser;
    }

    public String toJson() {
        try {
            ObjectMapper om = new ObjectMapper();
            return om.writeValueAsString(this);
        } catch (Exception e) {}
        return null;
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
        Crowded c = (Crowded) o;
        // field comparison
        return (this.getNameLocation().equals(c.getNameLocation()) &&
                this.getNUsers()== c.getNUsers());
    }
}
