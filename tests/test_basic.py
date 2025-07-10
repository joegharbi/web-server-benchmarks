#!/usr/bin/env python3
"""
Basic unit tests for web server benchmarks
"""

import unittest
import tempfile
import os
import json
from unittest.mock import patch, MagicMock

# Import functions to test
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from containers.measure_docker import parse_json_and_compute_energy as docker_parse_energy
from local.measure_local import parse_json_and_compute_energy as local_parse_energy

class TestEnergyParsing(unittest.TestCase):
    """Test energy data parsing functions"""
    
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        
    def tearDown(self):
        import shutil
        shutil.rmtree(self.temp_dir)
    
    def test_parse_json_and_compute_energy(self):
        """Test energy calculation from JSON data"""
        # Create sample JSON data
        sample_data = [
            {
                "consumers": [
                    {
                        "container": {"name": "test-container"},
                        "consumption": 1000000.0  # 1W in microwatts
                    }
                ]
            },
            {
                "consumers": [
                    {
                        "container": {"name": "test-container"},
                        "consumption": 2000000.0  # 2W in microwatts
                    }
                ]
            }
        ]
        
        json_file = os.path.join(self.temp_dir, "test.json")
        with open(json_file, 'w') as f:
            json.dump(sample_data, f)
        
        # Test energy calculation
        total_energy, avg_power, samples = docker_parse_energy(
            json_file, "test-container", 10.0
        )
        
        self.assertEqual(samples, 2)
        self.assertAlmostEqual(avg_power, 1.5)  # Average of 1W and 2W
        self.assertAlmostEqual(total_energy, 15.0)  # 1.5W * 10s

class TestConfiguration(unittest.TestCase):
    """Test configuration loading"""
    
    def test_auto_discovery_system(self):
        """Test that auto-discovery system is working"""
        # Test that the main directories exist
        self.assertTrue(os.path.exists("containers/static"))
        self.assertTrue(os.path.exists("containers/dynamic"))
        self.assertTrue(os.path.exists("web-socket"))
        
        # Test that run_benchmarks.sh exists (the auto-discovery script)
        self.assertTrue(os.path.exists("run_benchmarks.sh"))

class TestDockerImages(unittest.TestCase):
    """Test Docker image configurations"""
    
    def test_dockerfiles_exist(self):
        """Test that all Dockerfiles exist"""
        docker_dirs = [
            "containers/static",
            "containers/dynamic", 
            "web-socket"
        ]
        
        for docker_dir in docker_dirs:
            if os.path.exists(docker_dir):
                for item in os.listdir(docker_dir):
                    item_path = os.path.join(docker_dir, item)
                    if os.path.isdir(item_path):
                        dockerfile_path = os.path.join(item_path, "Dockerfile")
                        self.assertTrue(
                            os.path.exists(dockerfile_path),
                            f"Dockerfile missing in {item_path}"
                        )
    
    def test_auto_discovery_structure(self):
        """Test that auto-discovery directories have correct structure"""
        # Test that containers directories exist
        self.assertTrue(os.path.exists("containers/static"))
        self.assertTrue(os.path.exists("containers/dynamic"))
        self.assertTrue(os.path.exists("web-socket"))
        
        # Test that at least one Dockerfile exists in each directory
        static_has_dockerfile = any(
            os.path.exists(os.path.join("containers/static", d, "Dockerfile"))
            for d in os.listdir("containers/static")
            if os.path.isdir(os.path.join("containers/static", d))
        )
        self.assertTrue(static_has_dockerfile, "No Dockerfiles found in static containers")
        
        dynamic_has_dockerfile = any(
            os.path.exists(os.path.join("containers/dynamic", d, "Dockerfile"))
            for d in os.listdir("containers/dynamic")
            if os.path.isdir(os.path.join("containers/dynamic", d))
        )
        self.assertTrue(dynamic_has_dockerfile, "No Dockerfiles found in dynamic containers")

if __name__ == '__main__':
    unittest.main() 