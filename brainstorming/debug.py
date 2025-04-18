import subprocess
import time
import os

def start_scaphandre_container(scaphandre_image, scaphandre_params):
    # Mount current directory to /output in the container
    local_dir = os.getcwd()
    scaphandre_command = [
        "sudo", "docker", "run", "--privileged",
        "-v", "/sys/class/powercap:/sys/class/powercap",
        "-v", "/proc:/proc",
        "-v", f"{local_dir}:/output",  # Map local dir to /output
        scaphandre_image
    ] + scaphandre_params

    print(f"Running Scaphandre with command: {' '.join(scaphandre_command)}")
    
    scaphandre_process = subprocess.Popen(
        scaphandre_command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    return scaphandre_process

def debug_scaphandre_output(process, timeout=15):
    try:
        stdout, stderr = process.communicate(timeout=timeout)
        print("\nScaphandre STDOUT:")
        print(stdout)
        print("\nScaphandre STDERR:")
        print(stderr)
    except subprocess.TimeoutExpired:
        print(f"Process did not complete within {timeout} seconds, stopping gracefully...")
        process.send_signal(subprocess.SIGINT)  # Send Ctrl+C equivalent
        stdout, stderr = process.communicate(timeout=5)  # Wait for clean exit
        print("\nScaphandre STDOUT after stop:")
        print(stdout)
        print("\nScaphandre STDERR after stop:")
        print(stderr)

def main():
    scaphandre_image = "hubblo/scaphandre"
    output_file = "test.json"
    # Write to /output/test.json in container, mapped to local dir
    scaphandre_params = ["json", "-f", f"/output/{output_file}", "--timeout", "10"]  # Run for 10 seconds

    print("Starting Scaphandre container...")
    scaphandre_process = start_scaphandre_container(scaphandre_image, scaphandre_params)
    debug_scaphandre_output(scaphandre_process, timeout=15)

    time.sleep(2)  # Wait for file to be written
    if os.path.exists(output_file):
        print(f"\nSuccess: Output file '{output_file}' was created.")
        with open(output_file, "r") as f:
            print("Contents of test.json:")
            print(f.read())
    else:
        print(f"\nError: Output file '{output_file}' was not created.")

if __name__ == "__main__":
    main()