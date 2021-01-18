package common;

import com.fasterxml.jackson.annotation.JsonClassDescription;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

@JsonClassDescription
public class InfectedRatio {

        private String nameLocation;
        private float ratio;

        public InfectedRatio(){}

        @JsonCreator
        public InfectedRatio(@JsonProperty("name") String nameLocation, @JsonProperty("ratio") float ratio) {
            this.nameLocation = nameLocation;
            this.ratio = ratio;
        }

        @JsonProperty
        public String getNameLocation() {
            return nameLocation;
        }

        @JsonProperty
        public float getRatio() {
            return ratio;
        }

        @JsonProperty
        public void setNameLocation(String name) {
            this.nameLocation = name;
        }

        @JsonProperty
        public void setRacio(float racio){
            this.ratio = racio;
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
            InfectedRatio c = (InfectedRatio) o;
            // field comparison
            return (this.getNameLocation().equals(c.getNameLocation()) &&
                    this.getRatio()== c.getRatio());
        }
}


