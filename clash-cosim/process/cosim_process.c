/*C**********************************************************************
* FILENAME :        cosim_process.c      
*
* DESCRIPTION :
*       Executing a process and writing to files
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_process.h"

int tempName(char **name)
{
    if (*name != NULL)
    {
        printf("Nullpointer must be given to 'tempName'\n");
        return -1;
    }
    
    //#pragma clang diagnostic push
    //#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    *name = tempnam(".","clash");
    //#pragma clang diagnostic pop
    
    if (*name == NULL)
    {
        printf("Unable to create tempory file-name\n");
        return -1;
    }
    
    return 0;
}

int isDir(const char *path)
{
    struct stat path_stat;
    stat(path, &path_stat);
    return S_ISDIR(path_stat.st_mode);
}

int hasExtension(char const *name, char const *ext)
{
    size_t lenN = strlen(name);
    size_t lenE = strlen(ext);
    return lenN > lenE && strcmp(name + lenN - lenE, ext) == 0;
}

int getFiles(char *dir, struct coSimFiles **files, char const *ext, int *noFiles)
{
    DIR *directory = NULL;
    struct dirent *ent;

    if (dir == NULL || files == NULL)
    {
        printf("(C) null-pointer in 'get_files'");
        return -1;
    }
    
    directory = opendir (dir);
    if(directory == NULL)
    {
        printf("(C) unable to open dir in 'get_files'");
        return -1;
    }
    
    while ((ent = readdir (directory)) != NULL)
    {
        if(hasExtension(ent->d_name, ext))
        {
            (*files)->next = malloc (sizeof (struct coSimFiles));
            (*files) = (*files)->next;
            (*files)->next = NULL;
            (*files)->fileName = (char*) malloc((strlen(ent->d_name)+strlen(dir)+2) * sizeof(char));
            strcpy((*files)->fileName, dir);
            strcat((*files)->fileName, "/");
            strcat((*files)->fileName, ent->d_name);
            (*noFiles)++;
        }
    }
    
    if(closedir(directory) < 0)
        return -1;

    return 0;
}

int getAllFiles(struct coSimFiles **files, int noFiles, char **sourcePaths, char *extension)
{
    // variables
    int numberOfFiles, i;
    struct coSimFiles *current;
    
    // init
    numberOfFiles = 0;
    current = NULL;
    
    // check arguments
    if (sourcePaths == NULL)
    {
        printf("SourcePaths is Null in 'getAllFiles'\n");
        return -1;
    }
    if (*files != NULL)
    {
        printf("Files has to be Null in 'getAllFiles'\n");
        return -1;
    }
    
    for(i = 0; i<noFiles; i++)
    {
        // check if path exist
        if (access(sourcePaths[i], F_OK ) == -1)
        {
            printf("Given file does not exist: %s\n", sourcePaths[i]);
            return -1;
        }
        else
        {
            // check if is dir
            if(isDir(sourcePaths[i]))
            {
                getFiles(sourcePaths[i], &current, extension, &numberOfFiles);
            }
            else
            {
                // memory management
                if (*files == NULL)
                {
                    *files = (struct coSimFiles*) malloc (sizeof (struct coSimFiles));
                    current = *files;
                }
                else
                {
                    current->next = (struct coSimFiles*) malloc (sizeof (struct coSimFiles));
                    current = current->next;
                }
            
                // copy file-name
                current->fileName = (char*) malloc((strlen(sourcePaths[i])+1) * sizeof(char));
                strcpy(current->fileName, sourcePaths[i]);
                
                // update
                current->next = NULL;
                numberOfFiles++;
            }  
        }
    }
    
    // return
    return numberOfFiles;
}

/* Write data to file */
char *writeToFile(char *data)
{
    // variables
    int filedes, errno, error_;
    
    // init
    filedes = -1;
    error_ = 0;
    errno = 0;
    
    // check for null-pointer
    if (data == NULL)
    {
        printf("Null-pointer in 'write_to_file' function\n");
        return NULL;
    }
    
    // buffer to hold the temporary file name
    char *fileName = malloc(20 * sizeof(char));
    
    // Copy the relevant information in the buffers
    sprintf(fileName,"%s", "./clashCoSim-XXXXXX");
    
    // create the temporary file, this function will replace the 'X's
    if((filedes = mkstemp(fileName)) < 1)
    {
        printf("Creation of temp file failed with error [%s]\n",strerror(errno));
        error_ = 1;
    }
    else
    {
        // write data to file
        if(write(filedes,data,strlen(data)) == -1)
        {
            printf("Write failed with error [%s]\n",strerror(errno));
            error_ = 1;
        }
    }
    
    // free & close
    free(data);
    close(filedes);
    
    if (error_)
    {
        if (fileName != NULL) free(fileName);
        return NULL;
    }
    else
    {
        // return the name of created file
        return fileName;
    }
}

/* start a process */
int startProcess(char **arguments, struct CoSimComm **comm, int stdOut, int waitProcess)
{
    int fdNull;
    int pid, returnStatus; 
    struct CoSimComm *commExt = NULL;
    
    // check null-pointer
    if (arguments == NULL || arguments[0] == NULL)
    {
        printf("Arguments for creating process is null\n");
        return -1;
    }

    // set-up communication
    if (!waitProcess)
    {
        if (createComm(comm, &commExt) < 0) return -1;
    }
    
    // fork
    pid = fork();
    if (pid < 0)
    {
        printf("Unable to fork\n");
		
        if (!waitProcess)
        {
            // close pipes
            disposeComm(comm);
            disposeComm(&commExt);
        }
        
        // return 
        return -1;
    }
    else if (pid == 0)  
    {
        if (!waitProcess)
        {
            disposeComm(comm);
            if (shareComm(&commExt) < 0)
            {   
                disposeSharedComm();
                _exit(-1);
            }
        }
        
        // suppress std
        if (!stdOut)
        {
            // redirect to NULL
            fdNull = open("/dev/null", O_RDWR);
            dup2(fdNull, 1);
            if (fdNull > 2) close(fdNull);
        }
        
        // start process
        execvp(arguments[0], arguments);
        
        if (!waitProcess)
        {
            disposeSharedComm();
            _exit(-1);
        }
    }
    else
    {
        // wait child
        if (waitProcess)
        {
            waitpid(pid, &returnStatus, 0);
            if (returnStatus != 0)
            {
                printf("Error with creating process '%s'\n", arguments[0]);
                return -1;
            }
        }
        else
        {
            disposeComm(&commExt);
        }
    }
    
    return 0;
}